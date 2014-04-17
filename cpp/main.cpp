#include <iostream>
#include <string>

#include "jsoncons/json.hpp"
#include "utils/protocol.h"
#include "utils/connection.h"
#include "bots/bot_interface.h"
#include "bots/raw_bot.h"
#include "bots/basic/bot.h"
#include "bots/tomek/bot.h"
#include "bots/piotr/bot.h"

// Does not take ownership
bots::BotInterface* GetBot(const string& bot_name) {
  if (bot_name == "tomek")
    return new bots::tomek::Bot();
  if (bot_name == "piotr")
    return new bots::piotr::Bot();

  return new bots::basic::Bot();
}

void run(utils::Connection* connection, bots::RawBot* bot,
    const std::string& name, const std::string& key) {
  connection->send_requests({ utils::make_join(name, key) });

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

int main(int argc, const char* argv[]) {
  try {
    if (argc != 5) {
      std::cerr << "Usage: ./run host port botname botkey" << std::endl;
      return 1;
    }

    const std::string host(argv[1]);
    const std::string port(argv[2]);
    const std::string name(argv[3]);
    const std::string key(argv[4]);

    std::cout << "Host: " << host << ", port: " << port <<
      ", name: " << name << ", key:" << key << std::endl;

    std::unique_ptr<bots::RawBot> bot(new bots::RawBot(GetBot(name)));
    utils::Connection connection(host, port);

    run(&connection, bot.get(), name, key);
  } catch (const std::exception& e) {
    std::cerr << "EXCEPTION!!!" << std::endl;
    std::cerr << e.what() << std::endl;
    return 2;
  }

  return 0;
}
