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

std::string random_race_id() {
  char buffer[80];
  sprintf (buffer, "%d", rand());
  return std::string(buffer);
}

int main(int argc, char** argv) {
  FLAGS_throttle_scheduler = "WojtekThrottleScheduler";
  FLAGS_switch_scheduler = "NeverSwitchScheduler";

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
    options.track_name = "germany";
    result = simulator->Run(bot.get(), options);
  }

  std::cout << "BEST LAP: " << result.best_lap_time_in_ticks << std::endl;
  if (result.crashed) {
    std::cout << "CRASHED !!!!!!!!!!!!!" << std::endl;
  }
  std::cout << "MAX TICK TIME: " << result.max_tick_time_ms << " ms" << std::endl;

  return 0;
}
