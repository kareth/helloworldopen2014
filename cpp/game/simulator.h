#ifndef CPP_GAME_SIMULATOR_H_
#define CPP_GAME_SIMULATOR_H_

using jsoncons::json;

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
  struct Result {
    int best_lap_time_in_ticks = 100000;
    bool crashed = false;
    double total_distance = 0; // total distance drived
  };

  Result Run(bots::RawBot* raw_bot) {
    json game_init_json = json::parse_file("game/data/gameInitSuzuka.json");
    const auto& race_json = game_init_json["data"]["race"];
    Race race;
    race.ParseFromJson(race_json);

    CarTracker car_tracker(&race);
    CarState state = car_tracker.current_state();

    // Join
    // Game Init
    // OnCarPositions
    // Game Start
    // OnCarPositions*
    raw_bot->React(game_init_json);
    raw_bot->React(YourCar("red"));

    Result result;
    int current_lap = 0;
    int current_lap_ticks = 0;

    for (int i = 0; i < 1000; ++i) {
      current_lap_ticks++;
      auto response = raw_bot->React(CarPositionsFromState(state, i));
      Command command = CommandFromJson(response);
      CarState next_state = car_tracker.Predict(state, command);
      result.total_distance += car_tracker.DistanceBetween(state.position(), next_state.position());
      state = next_state;

      if (state.position().angle() > 60.0) {
        result.crashed = true;
      }

      if (state.position().lap() != current_lap) {
        current_lap = state.position().lap();
        result.best_lap_time_in_ticks = min(result.best_lap_time_in_ticks, current_lap_ticks);
        current_lap_ticks = 0;
      }
    }

    return result;
  }
 private:
  jsoncons::json CarPositionsFromState(const CarState& state, int game_tick) {
    jsoncons::json data;
    data["msgType"] = "carPositions";
    data["data"] = jsoncons::json(json::an_array);
    data["data"].add(jsoncons::json());
    data["data"][0]["id"] = jsoncons::json();
    data["data"][0]["id"]["color"] = "red";
    data["data"][0]["id"]["name"] = "Shumacher";
    data["data"][0]["angle"] = state.position().angle();
    data["data"][0]["piecePosition"] = jsoncons::json();
    data["data"][0]["piecePosition"]["pieceIndex"] = state.position().piece();
    data["data"][0]["piecePosition"]["inPieceDistance"] = state.position().piece_distance();
    data["data"][0]["piecePosition"]["lane"] = jsoncons::json();
    data["data"][0]["piecePosition"]["lane"]["startLaneIndex"] = state.position().start_lane();
    data["data"][0]["piecePosition"]["lane"]["endLaneIndex"] = state.position().end_lane();
    data["data"][0]["piecePosition"]["lap"] = state.position().lap();
    if (game_tick != -1) {
      data["gameTick"] = game_tick;
    }
    data["gameId"] = "ASDFASDFASDF";
    return data;
  }

  jsoncons::json YourCar(const string& color) {
    jsoncons::json data;
    data["msgType"] = "yourCar";
    data["data"] = jsoncons::json();
    data["data"]["color"] = color;
    return data;
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
