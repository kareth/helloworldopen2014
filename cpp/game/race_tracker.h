#ifndef CPP_GAME_RACE_TRACKER_H_
#define CPP_GAME_RACE_TRACKER_H_

namespace game {

class RaceTracker {
 public:
  RaceTracker(const game::Race& race, const std::string& color);

  void Record(const std::map<std::string, game::Position>& positions);

  void RecordLapTime(const std::string& color, int time);

  // Resets positions, keeps speed stats.
  // TODO is it necessary?
  void Reset() {}

  const EnemyTracker& enemy(const std::string& color) const { return enemies_.at(indexes_.at(color)); }

  int TimeToReach(int piece, double position);

  Position BumpPosition(const std::string& color);

 private:
  void RecordEnemy(int index, const game::Position& position);
  void UpdateSpeedStats(int index, const game::Position& position);

  std::vector<EnemyInfo> enemies_;

  std::map<std::string, int> indexes_;
  std::string color_;
};

}  // namespace game

#endif  // CPP_GAME_RACE_TRACKER_H_
