#ifndef CPP_UTILS_CONNECTION_H_
#define CPP_UTILS_CONNECTION_H_

#include <boost/asio.hpp>
#include <string>
#include <iostream>
#include "jsoncons/json.hpp"

using boost::asio::ip::tcp;

namespace utils {

class Connection {
 public:
  Connection(const std::string& host, const std::string& port);
  ~Connection();
  jsoncons::json receive_response(boost::system::error_code* error);
  void send_requests(const std::vector<jsoncons::json>& msgs);

 private:
  jsoncons::json history_;
  boost::asio::io_service io_service;
  tcp::socket socket;
  boost::asio::streambuf response_buf;
};

}  // namespace utils

#endif  // CPP_UTILS_CONNECTION_H_
