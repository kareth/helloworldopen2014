#include "game/lane_scorer.h"

#include "game/race_tracker.h"

DECLARE_bool(log_overtaking);

namespace game {

LaneScorer::LaneScorer(const Race& race,
                       CarTracker& car_tracker,
                       RaceTracker& race_tracker,
                       std::vector<EnemyTracker>& enemies,
                       const string& color)
  : race_(race), car_tracker_(car_tracker), enemies_(enemies), color_(color),
    race_tracker_(race_tracker), kCarLength(race.cars()[0].length()) {
}

int LaneScorer::ScoreLane(int from, int to, int lane) {
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

  int lane_score = 0;
  for (auto& enemy : enemies_) {
    if (enemy.color() == color_) continue;  // Me
    int score = ScoreEnemy(me, enemy, end_position);

    if (FLAGS_log_overtaking)
      printf("Score of %s is %d\n",enemy.color().c_str(), score);

    if (score < 0)  // If we should overtake someone
      lane_score = min(lane_score, score);

    if (score > 0 && lane_score >= 0)  // If there is s1 worth bumping
      lane_score = max(lane_score, score);
  }
  return lane_score;
}

int LaneScorer::ScoreEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  // TODO if im on the edge of that difference, I can switch into him.
  // TODO2 - I should also check the end so I wont hit him at the end
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

int LaneScorer::ScoreDeadEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  auto safe_position = car_tracker_.PredictPosition(enemy.state().position(), 3 * kCarLength);

  // Wont reach safe_position before spawn
  if (!me.IsReady() || me.TimeToPosition(safe_position) > enemy.time_to_spawn()) {
    return (int) kDeadCrash;
  } else {
    return 0;
  }
}

int LaneScorer::ScoreLivingEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  // If models are ready try to predict if we will actually hit
  if (enemy.IsReady() && me.IsReady()) {
    Position bump_position;
    bool will_bump = BumpPosition(me, enemy, end_position, &bump_position);

    if (will_bump) {
      return EnemyBumpScore(enemy, me.ExpectedVelocity(bump_position), enemy.ExpectedVelocity(bump_position));
    } else {
      return 0;
    }
  } else {
    return EnemyBumpScore(enemy, me.state().velocity(), enemy.state().velocity());
  }
}

int LaneScorer::EnemyBumpScore(const EnemyTracker& enemy, double my_speed, double his_speed) {
  if (his_speed < 0.95 * my_speed) {
    return (-kDeadCrash) * (1 - his_speed / my_speed);
  } else {
    return 0;
  }
  //if (race_tracker_.IsCompetitive(enemy.color()))
  //  return 10;
}

bool LaneScorer::BumpPosition(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position, Position* bump_position) {
  int my_time = me.TimeToPosition(end_position);
  int enemy_time = enemy.TimeToPosition(
      car_tracker_.PredictPosition(end_position, kCarLength * 1.1));

  if (my_time > enemy_time)
    return false;

  // TODO OPTIMIZE - steps ^ 2
  for (int i = 0; i < enemy_time; i++) {
    auto my_position = me.PositionAfterTime(i, end_position.end_lane());
    auto enemy_position = enemy.PositionAfterTime(i, end_position.end_lane());

    if (car_tracker_.DistanceBetween(my_position, enemy_position) < kCarLength) {
      *bump_position = enemy_position;
      return true;
    }
  }

  // We havent found bump (shouldnt happen)
  return false;
}

}  // namespace game
