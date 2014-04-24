#include <iostream>
#include <string>

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

DEFINE_string(track, "", "The track to join the race. Possible options: keimola, germany, usa.");
DEFINE_int32(num_players, 1, "The number of players that will race (including you)");
DEFINE_string(bot, "tomek", "The bot to use");

DEFINE_string(host, "testserver.helloworldopen.com", "");
DEFINE_string(port, "8091", "");
DEFINE_string(key, "", "");

// Does not take ownership
bots::BotInterface* GetBot(const string& bot_name) {
  if (bot_name == "tomek")
    return new bots::tomek::Bot();
  if (bot_name == "piotr")
    return new bots::piotr::Bot();
  if (bot_name == "greedy")
    return new bots::greedy::Bot();
  if (bot_name == "stepping")
    return new bots::stepping::Bot();
  if (bot_name == "Need for C")
    return new bots::stepping::Bot();

  return new bots::basic::Bot();
}

void run(utils::Connection* connection, bots::RawBot* bot,
    const std::string& name, const std::string& key) {
  if (!FLAGS_track.empty()) {
    connection->send_requests(
        { utils::make_join_race("NFC-" + name, key, FLAGS_track, FLAGS_num_players) });
  } else {
    connection->send_requests({ utils::make_join("NFC-" + name, key) });
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

int main(int argc, char** argv) {
  gflags::ParseCommandLineFlags(&argc, &argv, true);

  try {
    std::cout << "Host: " << FLAGS_host << ", port: " << FLAGS_port <<
      ", name: " << FLAGS_bot << ", key:" << FLAGS_key << std::endl;

    std::unique_ptr<bots::RawBot> bot(new bots::RawBot(GetBot(FLAGS_bot)));
    utils::Connection connection(FLAGS_host, FLAGS_port);

    run(&connection, bot.get(), FLAGS_bot, FLAGS_key);
  } catch (const std::exception& e) {
    std::cerr << "EXCEPTION!!!" << std::endl;
    std::cerr << e.what() << std::endl;
    return 2;
  }

  return 0;
}
