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
#include "bots/switch_optimizer/bot.h"

DEFINE_string(track, "", "The track to join the race. Possible options: keimola, germany, usa.");
DEFINE_int32(num_players, 1, "The number of players that will race (including you)");
DEFINE_string(bot, "", "The bot name.");
DEFINE_string(bot_algorithm, "stepping", "The bot algorithm to use.");
DECLARE_string(race_id);

DEFINE_string(host, "testserver.helloworldopen.com", "");
DEFINE_string(port, "8091", "");
DEFINE_string(key, "", "");

// Does not take ownership
bots::BotInterface* GetBot(const string& bot_algorithm) {
  if (bot_algorithm == "tomek")
    return new bots::tomek::Bot();
  if (bot_algorithm == "piotr")
    return new bots::piotr::Bot();
  if (bot_algorithm == "greedy")
    return new bots::greedy::Bot();
  if (bot_algorithm == "stepping")
    return new bots::stepping::Bot();
  if (bot_algorithm == "constant")
    return new bots::constant::Bot();
  if (bot_algorithm == "kamikaze")
    return new bots::kamikaze::Bot();
  if (bot_algorithm == "switch_optimizer")
    return new bots::switch_optimizer::Bot();

  return new bots::basic::Bot();
}

void run(utils::Connection* connection, bots::RawBot* bot,
    const std::string& name, const std::string& key) {
  std::string bot_name = name;

  if (bot_name.empty()) {
    bot_name = "NFC-" + FLAGS_race_id;
  }

  if (!FLAGS_track.empty()) {
    connection->send_requests(
        { utils::make_join_race(bot_name, key, FLAGS_track, FLAGS_num_players) });
  } else {
    connection->send_requests({ utils::make_join(bot_name, key) });
  }

  for (;;) {
    boost::system::error_code error;
    auto response = connection->receive_response(&error);

    if (error == boost::asio::error::eof) {
      std::cout << "Connection closed" << std::endl;
      break;
    } else if (error) {
      throw boost::system::system_error(error);
    }

    connection->send_requests(bot->React(response));
  }
}

std::string random_race_id() {
  char buffer[80];
  sprintf (buffer, "%d", rand());
  return std::string(buffer);
}

int main(int argc, char** argv) {
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  srand (time(NULL));

  if (FLAGS_race_id.empty()) {
    FLAGS_race_id = random_race_id();
  }
  std::cout << "Race id: " << FLAGS_race_id << std::endl;
  boost::filesystem::create_directories("bin/" + FLAGS_race_id);

  try {
    std::cout << "Host: " << FLAGS_host << ", port: " << FLAGS_port <<
      ", name: " << FLAGS_bot << ", key:" << FLAGS_key << std::endl;

    std::unique_ptr<bots::RawBot> bot(new bots::RawBot(GetBot(FLAGS_bot_algorithm)));
    utils::Connection connection(FLAGS_host, FLAGS_port);

    run(&connection, bot.get(), FLAGS_bot, FLAGS_key);
  } catch (const std::exception& e) {
    std::cerr << "EXCEPTION!!!" << std::endl;
    std::cerr << e.what() << std::endl;
    return 2;
  }

  return 0;
}
