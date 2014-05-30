#include "game/lane_scorer.h"

#include "game/race_tracker.h"

namespace game {

LaneScorer::LaneScorer(const Race& race,
                       CarTracker& car_tracker,
                       RaceTracker& race_tracker,
                       std::vector<EnemyTracker>& enemies,
                       const string& color)
  : race_(race), car_tracker_(car_tracker), enemies_(enemies), color_(color),
    race_tracker_(race_tracker), kCarLength(race.cars()[0].length()) {
}

LaneScore LaneScorer::ScoreLane(int from, int to, int lane) {
  using std::max;
  using std::min;
  auto& me = *std::find_if(enemies_.begin(), enemies_.end(), [this](const EnemyTracker& e){ return e.color() == this->color_; });

  // The position for me to do another switch later safely
  Position end_position;
  end_position.set_piece(to);
  end_position.set_piece_distance(0);
  end_position.set_end_lane(lane);

  int lane_score = 0;
  for (auto& enemy : enemies_) {
    if (enemy.color() == color_) continue; // Me
    int score = ScoreEnemy(me, enemy, end_position);

    // If we should overtake someone
    if (score > 0)
      lane_score = max(lane_score, score);

    // If there is s1 worth bumping
    if (score < 0 && lane_score <= 0)
      lane_score = min(lane_score, score);
  }
  return LaneScore(lane_score);
}

// = 0 - ignore
// > 0 - competitive, should bump
// < 0 - slow, should overtake
//<<<0 - killing. (??)
int LaneScorer::ScoreEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  // if im closer to the end, then im ahead, so ignore the guy
  // TODO if im on the edge of that difference, I can switch into him.
  if (car_tracker_.DistanceBetween(me.state().position(), end_position) <
      car_tracker_.DistanceBetween(enemy.state().position(), end_position))
    return 0;

  if (enemy.has_finished()) {
    return 0;

  } else if (enemy.is_dead()) {
    return ScoreDeadEnemy(me, enemy, end_position);

  } else if (enemy.is_accelerating()) {
    return (int) kDeadCrash;

  } else {
    return ScoreLivingEnemy(me, enemy, end_position);
  }
}

// If enemy is dead we must check if he threatens us
int LaneScorer::ScoreDeadEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  auto safe_position = car_tracker_.PredictPosition(enemy.state().position(), 2 * kCarLength);

  // Wont reach safe_position before spawn
  if (me.TimeToPosition(safe_position) > enemy.time_to_spawn()) {
    return (int) kDeadCrash;
  } else {
    return 0;
  }
}

int LaneScorer::ScoreLivingEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  int my_time = me.TimeToPosition(end_position);
  int enemy_time = enemy.TimeToPosition(
      car_tracker_.PredictPosition(end_position, kCarLength));

  if (my_time > enemy_time)
    return 0;

  // Otherwise we will bump
  return EnemyBumpScore(me, enemy, end_position);
}

// TODO score:D
int LaneScorer::EnemyBumpScore(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position) {
  // Overtake
  if (race_tracker_.ShouldOvertake(enemy.color(), me.state().position().piece(), end_position.piece())) {
    // TODO expected crash speed
    return (int) kOvertake;
  // Bump
  } else {
    // TODO?
    return 0;
  }
}

}  // namespace game
