#include "game/lane_scorer.h"

#include "game/race_tracker.h"
#include "utils/stopwatch.h"

DECLARE_bool(log_overtaking);
DECLARE_bool(continuous_integration);
DECLARE_double(overtake_treshold);
DEFINE_int32(bumps_to_overtake, 3, "Bumps to overtake");
DEFINE_int32(ticks_to_overtake, 100, "ticks span to count bumps");

namespace game {

LaneScorer::LaneScorer(const Race& race,
                       CarTracker& car_tracker,
                       RaceTracker& race_tracker,
                       std::vector<EnemyTracker>& enemies,
                       const string& color)
  : race_(race), car_tracker_(car_tracker), enemies_(enemies), color_(color),
    race_tracker_(race_tracker), kCarLength(race.cars()[0].length()) {
}

LaneScorer::~LaneScorer() {
  printf("Lane scorer longest computation time: %lf ms\n", longest_calculation_time_);
}

double LaneScorer::ScoreLane(int from, int to, int lane, const utils::Deadline& deadline) {
  utils::StopWatch timer;
  using std::max; using std::min;
  auto& me = *std::find_if(enemies_.begin(), enemies_.end(), [this](const EnemyTracker& e){ return e.color() == this->color_; });

  Position end_position(to, 0);
  end_position.set_start_lane(lane);
  end_position.set_end_lane(lane);

  if (FLAGS_log_overtaking)
    printf ("(%d,%.2lf,<%d,%d>) CEL\n", end_position.piece(),
                                        end_position.piece_distance(),
                                        end_position.start_lane(),
                                        end_position.end_lane());

  my_prediction_calculated_ = false;
  int lane_score = 0;
  for (auto& enemy : enemies_) {
    if (enemy.color() == color_) continue;  // Me
    if (deadline.HasExpired()) {
      printf("Lane score: Duration expired\n");
      return lane_score;
    }

    int score = ScoreEnemy(me, enemy, end_position);

    if (FLAGS_log_overtaking)
      printf("Score of %s is %d\n",enemy.color().c_str(), score);

    if (score < 0)  // If we should overtake someone
      lane_score = min(lane_score, score);

    if (score > 0 && lane_score >= 0)  // If there is s1 worth bumping
      lane_score = max(lane_score, score);
  }
  if (FLAGS_continuous_integration)
    printf("Lane %d scored in %lf ms\n", lane, timer.elapsed());

  longest_calculation_time_ = max(longest_calculation_time_, timer.elapsed());
  return lane_score;
}

double LaneScorer::ScoreEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  if (FLAGS_log_overtaking)
    printf ("(%d,%.2lf,<%d,%d> =%.2lf) (%d,%.2lf,<%d,%d> =%.2lf) ",
        me.state().position().piece(),
        me.state().position().piece_distance(),
        me.state().position().start_lane(),
        me.state().position().end_lane(),
        car_tracker_.DistanceBetween(me.state().position(), end_position),
        enemy.state().position().piece(),
        enemy.state().position().piece_distance(),
        enemy.state().position().start_lane(),
        enemy.state().position().end_lane(),
        car_tracker_.DistanceBetween(enemy.state().position(), end_position));

  // if im closer to the end, then im ahead, so ignore the guy
  //
  // Clarification in case of switching into him:
  // Its If we are closer to target, in case of switching into him,
  // we will most probably land ahead of him.
  // If we would take such car seriously, we would never have safe switch
  // if there is s1 on another lane
  if (car_tracker_.DistanceBetween(me.state().position(), end_position) <
      car_tracker_.DistanceBetween(enemy.state().position(), end_position))
    return 0;

  if (enemy.has_finished()) {
    return 0;
  }
  else if (enemy.is_dead()) {
    return ScoreDeadEnemy(me, enemy, end_position);
  }
  else {
    return ScoreLivingEnemy(me, enemy, end_position);
  }
}

double LaneScorer::ScoreDeadEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  if (me.IsThreatToMe(enemy))
    return int(kDeadCrash);
  else
    return 0;
}

double LaneScorer::ScoreLivingEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  // If models are ready try to predict if we will actually hit
  if (enemy.IsReady() && me.IsReady()) {
    Position bump_position;
    bool will_bump = BumpPosition(me, enemy, end_position, &bump_position);

    if (will_bump) {
      if (FLAGS_log_overtaking)
        printf(" dd %lf %lf bb \n", me.ExpectedVelocity(bump_position), enemy.ExpectedVelocity(bump_position));
      return EnemyBumpScore(me, enemy, me.ExpectedVelocity(bump_position), enemy.ExpectedVelocity(bump_position));
    } else {
      return 0;
    }
  } else {
    return EnemyBumpScore(me, enemy, me.state().velocity(), enemy.state().velocity());
  }
}

double LaneScorer::EnemyBumpScore(const EnemyTracker& me, const EnemyTracker& enemy, double my_speed, double his_speed) {
  if (his_speed < FLAGS_overtake_treshold * my_speed) {
    if (my_speed == 0)
      return 0;
    return double(kDeadCrash) * (1.0 - his_speed / my_speed);
  } else {
    if (race_tracker_.bump_detector().BumpsBetween(me.color(), enemy.color(), FLAGS_ticks_to_overtake) >=
        FLAGS_bumps_to_overtake) {
      return -5;
    }
    // Check for minimal speed
    return 0;
  }
  // if (race_tracker_.IsCompetitive(enemy.color()))
  //   return 10;
}

bool LaneScorer::BumpPosition(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position, Position* bump_position) {
  if (!my_prediction_calculated_) {
    my_prediction_.clear();
    my_time_ = me.TimeToPosition(end_position, &my_prediction_);
    my_prediction_calculated_ = true;
  }

  /*std::cout << " end_position: " << end_position.ShortDebugString() << std::endl
            << " enemy_target: " << car_tracker_.PredictPosition(end_position, kCarLength * 1.1).ShortDebugString() << std::endl
            << " me: " << me.state().position().ShortDebugString() << std::endl
            << " enemy: " << enemy.state().position().ShortDebugString() << std::endl
            << std::endl;*/

  vector<Position> enemy_prediction;
  int enemy_time = enemy.TimeToPosition(car_tracker_.PredictPosition(end_position, kCarLength * 1.1), &enemy_prediction);

  //printf("Predicting bump position, %d and %d steps done.\n", my_time, enemy_time);
  if (FLAGS_log_overtaking)
    printf(" {{%d %d}} ", my_time_, enemy_time);

  if (my_time_ > enemy_time)
    return false;

  // TODO can make binsearch here, worth it?
  for (int i = 0; i < min(my_prediction_.size(), enemy_prediction.size()); i++) {
    auto my_position = my_prediction_[i];
    auto enemy_position = enemy_prediction[i];

    if (car_tracker_.DistanceBetween(my_position, enemy_position, nullptr, kCarLength) < kCarLength) {
      *bump_position = enemy_position;
      return true;
    }
  }

  return false;  // We havent found bump (shouldnt happen)
}

}  // namespace game
