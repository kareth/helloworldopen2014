#ifndef CPP_GAME_SIMULATOR_H_
#define CPP_GAME_SIMULATOR_H_

#include <fstream>
#include <chrono>

using jsoncons::json;

namespace {
  template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
  }

  typedef std::chrono::high_resolution_clock clock;
  typedef std::chrono::microseconds microseconds;
 
  clock::time_point now(){return clock::now();}
 
  class StopWatch
  {
    clock::time_point start_;
   public:
    StopWatch() : start_(clock::now()) {}
    double elapsed() { 
      return std::chrono::duration_cast<microseconds>(now() - start_).count() / 1000.0;
    }
  };
 
} // whatever

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
    // The best lap measured in ticks.
    int best_lap_time_in_ticks = 100000;

    // The maximum time in ms it took to compute the next command.
    double max_tick_time_ms = 0;
    double avg_tick_time_ms = 0;
    int total_ticks = 0;

    // True if car crashed during simulation.
    bool crashed = false;
    double total_distance = 0; // total distance drived
  };

  struct Options {
    // The track name used that simulation will take place on.
    //
    // Possible values are files inside game/data/maps/*.json:
    // - elaeintarha
    // - england
    // - france
    // - germany
    // - imola
    // - keimola
    // - suzuka
    // - usa
    string track_name = "keimola";

    int max_ticks_to_simulate = 10000;
    int max_laps_to_simulate = 3;
  };

  const string kCarColor = "red";

  Result Run(bots::RawBot* raw_bot, const Options& options) {
    json game_init_json = GameInitMessage(options);
    const auto& race_json = game_init_json["data"]["race"];
    Race race;
    race.ParseFromJson(race_json);

    CarTracker car_tracker(&race);
    CarState state = car_tracker.current_state();

    raw_bot->React(YourCar(kCarColor));
    raw_bot->React(game_init_json);

    Result result;
    int current_lap = 0;
    int current_lap_ticks = 0;
    int total_ticks = 0;

    double max_tick_time_ms = 0;
    double sum_tick_time_ms = 0;

    std::ofstream ofs("data.csv", std::ofstream::out);
    ofs << "tick," << "x," << "turbo," << "a," << "v," << "d," << "dir," << "rad," << "tick_time" << std::endl;
    for (int i = 0; i < options.max_ticks_to_simulate; ++i) {
      current_lap_ticks++;
      total_ticks++;


      StopWatch stopwatch;
      auto response = raw_bot->React(CarPositionsFromState(state, i));
      double current_tick_time_ms = stopwatch.elapsed();
      max_tick_time_ms = fmax(max_tick_time_ms, current_tick_time_ms);
      sum_tick_time_ms += current_tick_time_ms;

      Command command = CommandFromJson(response);

      Piece piece = race.track().pieces()[state.position().piece()];
      ofs << total_ticks << ',' << command.TurboSet() << ',' << command.throttle() << ','
          << state.position().angle() << ',' << state.velocity()  << ','
          << result.total_distance << ',' << -sgn(piece.angle()) << ','
          << piece.radius() << ',' << current_tick_time_ms << std::endl;
      ofs.flush();

      CarState next_state = car_tracker.Predict(state, command);
      result.total_distance += car_tracker.DistanceBetween(state.position(), next_state.position());
      state = next_state;

      if (fabs(state.position().angle()) > 60.0) {
        result.crashed = true;

        raw_bot->React(CrashMessage());
        Position position = state.position();
        position.set_angle(0);
        state = CarState(position);
        raw_bot->React(SpawnMessage());
      }

      if (state.position().lap() != current_lap) {
        raw_bot->React(LapFinishedMessage(current_lap, current_lap_ticks));

        current_lap = state.position().lap();
        result.best_lap_time_in_ticks = min(result.best_lap_time_in_ticks, current_lap_ticks);
        current_lap_ticks = 0;

        if (current_lap == options.max_laps_to_simulate) break;
      }
    }

    ofs.close();

    result.max_tick_time_ms = max_tick_time_ms;
    result.avg_tick_time_ms = sum_tick_time_ms / total_ticks;
    result.total_ticks = total_ticks;

    return result;
  }

 private:
  jsoncons::json CrashMessage() {
    jsoncons::json data;
    data["msgType"] = "crash";
    data["data"] = jsoncons::json();
    data["data"]["color"] = kCarColor;
    return data;
  }

  jsoncons::json SpawnMessage() {
    jsoncons::json data;
    data["msgType"] = "spawn";
    data["data"] = jsoncons::json();
    data["data"]["color"] = kCarColor;
    return data;
  }

  jsoncons::json LapFinishedMessage(int lap, int lap_ticks) {
    jsoncons::json data;
    data["msgType"] = "lapFinished";
    data["data"] = jsoncons::json();
    data["data"]["car"] = jsoncons::json();
    data["data"]["car"]["color"] = kCarColor;

    // TODO(tomek) This is not perfect (we use ticks instead of millis
    //             and raceTime and ranking are incorrect).
    data["data"]["lapTime"] = jsoncons::json();
    data["data"]["lapTime"]["lap"] = lap;
    data["data"]["lapTime"]["millis"] = lap_ticks;

    data["data"]["raceTime"] = jsoncons::json();
    data["data"]["raceTime"]["laps"] = lap;
    data["data"]["raceTime"]["millis"] = lap_ticks;

    data["data"]["ranking"] = jsoncons::json();
    data["data"]["ranking"]["fastestLap"] = lap;
    data["data"]["ranking"]["overall"] = 1;
    return data;
  }

  jsoncons::json GameInitMessage(const Options& options) {
    auto track = json::parse_file("../game/data/maps/" + options.track_name + ".json");
    jsoncons::json data;
    data["gameId"] = "83abef1f-e601-438b-ad80-7f6e2f7acdd7";
    data["msgType"] = "gameInit";
    data["data"] = jsoncons::json();
    data["data"]["race"] = jsoncons::json();

    data["data"]["race"]["cars"] = jsoncons::json(json::an_array);
    data["data"]["race"]["cars"].add(jsoncons::json());
    data["data"]["race"]["cars"][0]["dimensions"] = jsoncons::json();
    data["data"]["race"]["cars"][0]["dimensions"]["guideFlagPosition"] = 10.0;
    data["data"]["race"]["cars"][0]["dimensions"]["length"] = 40.0;
    data["data"]["race"]["cars"][0]["dimensions"]["width"] = 20.0;
    data["data"]["race"]["cars"][0]["id"] = jsoncons::json();
    data["data"]["race"]["cars"][0]["id"]["color"] = kCarColor;
    data["data"]["race"]["cars"][0]["id"]["name"] = "Need for C";

    data["data"]["race"]["raceSession"] = jsoncons::json();
    data["data"]["race"]["raceSession"]["laps"] = options.max_laps_to_simulate;
    data["data"]["race"]["raceSession"]["maxLapTimeMs"] = 60000;
    data["data"]["race"]["raceSession"]["quickRace"] = true;

    data["data"]["race"]["track"] = track;
    return data;
  }

  jsoncons::json CarPositionsFromState(const CarState& state, int game_tick) {
    jsoncons::json data;
    data["msgType"] = "carPositions";
    data["data"] = jsoncons::json(json::an_array);
    data["data"].add(jsoncons::json());
    data["data"][0]["id"] = jsoncons::json();
    data["data"][0]["id"]["color"] = kCarColor;
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
