#ifndef CPP_PHYSICS_SPEED_TRACKER_H_
#define CPP_PHYSICS_SPEED_TRACKER_H_

#include <map>
#include <string>
#include "game/position.h"
#include "game/race.h"

using std::string;
using std::map;

namespace physics {

class SpeedTracker {
 public:
  explicit SpeedTracker(const game::Race& race);
  void Update(const map<string, game::Position>& positions);

  double SpeedFor(const string& color) const { return speeds_.at(color); }

 private:
  const game::Race& race_;
  map<string, double> speeds_;
  map<string, game::Position> last_positions_;
};

}  // namespace physics

#endif  // CPP_PHYSICS_SPEED_TRACKER_H_
