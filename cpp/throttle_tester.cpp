#include <iostream>
#include <string>
#include <ctime>
#include <cstdio>
#include <boost/filesystem.hpp>

#include "gflags/gflags.h"
#include "jsoncons/json.hpp"
#include "utils/protocol.h"
#include "utils/connection.h"
#include "bots/bot_interface.h"
#include "bots/raw_bot.h"
#include "bots/basic/bot.h"
#include "bots/tomek/bot.h"
#include "bots/piotr/bot.h"
#include "bots/greedy/bot.h"
#include "bots/stepping/bot.h"
#include "bots/constant/bot.h"
#include "bots/kamikaze/bot.h"
#include "game/simulator.h"

DECLARE_string(race_id);
DECLARE_string(throttle_scheduler);
DECLARE_string(switch_scheduler);
DEFINE_string(track, "keimola", "The track to run simulator on.");
DEFINE_int32(physics, 0, "The physics to use");

std::string random_race_id() {
  char buffer[80];
  sprintf (buffer, "%d", rand());
  return std::string(buffer);
}

namespace {

double range(double a, double b) {
  double r = double(rand() % 1000000) / 1000000.0;
  return a + r * (b-a);
}

}  // anonymous namespace

int main(int argc, char** argv) {
  std::vector<std::pair<std::vector<double>, std::vector<double> > > physics;
  physics = {
    {{0.98, 0.2     }, {1.9, -0.9, -0.00125, 0.00125 * sqrt(180000), 0.3}},
    {{0.98, 0.168896}, {1.9, -0.9, -0.00118604, 0.483757, 0.284649}},
    {{0.98, 0.233986}, {1.9, -0.9, -0.00120513, 0.497467, 0.289232}},
    {{0.98, 0.214027}, {1.9, -0.9, -0.00127153, 0.546417, 0.305167}},
    {{0.98, 0.17629 }, {1.9, -0.9, -0.00130643, 0.572936, 0.313544}},
    {{0.98, 0.198433}, {1.9, -0.9, -0.00129546, 0.56454 , 0.31091 }},
    {{0.98, 0.210197}, {1.9, -0.9, -0.00121806, 0.506844, 0.292334}},
  };

  srand(123456789);
  while (physics.size() < 100) {
    //                  speed_decay       acceleration       angle_decay angle_change_speed     target_angle_fac target_angle_treshold
    physics.push_back({{range(0.9, 0.99), range(0.1, 0.5)}, {1.9, -0.9, range(-0.001, -0.002), range(0.3, 0.7), range(0.25, 0.35)}});

    /*printf("{%lf %lf}{%lf %lf %lf %lf}\n",
        physics.back().first[0],
        physics.back().first[1],
        physics.back().second[0],
        physics.back().second[1],
        physics.back().second[2],
        physics.back().second[3]
        );*/
  }

  // FLAGS_throttle_scheduler = "BinaryThrottleScheduler";
  // FLAGS_switch_scheduler = "NeverSwitchScheduler";

  gflags::ParseCommandLineFlags(&argc, &argv, true);
  srand (time(NULL));

  if (FLAGS_race_id.empty()) {
    FLAGS_race_id = random_race_id();
  }
  boost::filesystem::create_directories("bin/" + FLAGS_race_id);

  game::Simulator::Result result;
  {
    std::unique_ptr<bots::RawBot> bot(new bots::RawBot(new bots::stepping::Bot()));
    std::unique_ptr<game::Simulator> simulator(new game::Simulator());

    game::Simulator::Options options;
    options.physics_params = game::PhysicsParams::Load();
    options.physics_params.velocity_model_params.model = physics[FLAGS_physics].first;
    options.physics_params.drift_model_params.model = physics[FLAGS_physics].second;

    options.track_name = FLAGS_track;
    result = simulator->Run(bot.get(), options);
  }

  std::cout << "BEST LAP: " << result.best_lap_time_in_ticks << std::endl;
  std::cout << "TOTAL TICKS: " << result.total_ticks << std::endl;
  std::cout << "TOTAL DISTANCE: " << result.total_distance << std::endl;
  std::cout << "MAX TICK TIME: " << result.max_tick_time_ms << " ms" << std::endl;
  std::cout << "AVG TICK TIME: " << result.avg_tick_time_ms << " ms" << std::endl;
  if (result.crashed) {
    std::cout << "CRASHED !!!!!!!!!!!!!" << std::endl;
  }

  return 0;
}
