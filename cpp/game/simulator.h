#ifndef CPP_GAME_SIMULATOR_H_
#define CPP_GAME_SIMULATOR_H_

namespace game {

// What we need
// - end condition (number of ticks, laps, crash, etc.)
// - race
// - raw bot (or multiple!)
// - starting state?
// - physics config (drift, velocity, switches)
// - respawn config
// - turbo config

class Simulator {
 public:
  void Simulate(const Race& race, int game_ticks, bots::RawBot* raw_bot) {
    CarTracker car_tracker(&race);
    CarState state = car_tracker.current_state;

    // Join
    // Game Init
    // OnCarPositions
    // Game Start
    // OnCarPositions*
    raw_bot->React(...);

    for (int i = 0; i < game_ticks; ++i) {
      auto response = raw_bot->React(CarPositionsFromState(state));
      Command command = CommandFromJson(response);
      state = car_tracker.Predict(state, command);
    }
  }

  jsoncons::json CarPositionsFromState(const CarState& state) {

  }

  Command CommandFromJson(const std::vector<jsoncons::json>& response) {
    if (response.empty()) return Command();
    if (response[0]["msgType"].as_string() == "throttle")
      return Command(response[0]["data"].as_double());

    if (response[0]["msgType"].as_string() == "turbo")
      return Command::Turbo();

    if (response[0]["msgType"].as_string() == "switchLane") {
      if (response[0]["data"] == "Left") {
        return Command(Switch::kSwitchLeft);
      } else {
        return Command(Switch::kSwitchRight);
      }
    }

    return Command();
  }
};

}  // namespace game

#endif  // CPP_GAME_SIMULATOR_H_
