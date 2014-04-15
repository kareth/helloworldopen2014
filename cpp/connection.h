#ifndef HWO_CONNECTION_H
#define HWO_CONNECTION_H

#include <string>
#include <iostream>
#include <boost/asio.hpp>
#include <jsoncons/json.hpp>

using boost::asio::ip::tcp;

class hwo_connection
{
public:
  hwo_connection(const std::string& host, const std::string& port);
  ~hwo_connection();
  jsoncons::json receive_response(boost::system::error_code& error);
  void send_requests(const std::vector<jsoncons::json>& msgs);

private:
  boost::asio::io_service io_service;
  tcp::socket socket;
  boost::asio::streambuf response_buf;
};

#endif