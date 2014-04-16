#include "utils/connection.h"

namespace utils {

Connection::Connection(const std::string& host, const std::string& port)
  : socket(io_service) {
  tcp::resolver resolver(io_service);
  tcp::resolver::query query(host, port);
  boost::asio::connect(socket, resolver.resolve(query));
  response_buf.prepare(8192);
}

Connection::~Connection() {
  socket.close();
}

jsoncons::json Connection::receive_response(boost::system::error_code* error) {
  boost::asio::read_until(socket, response_buf, "\n", *error);
  if (*error) {
    return jsoncons::json();
  }
  std::istream s(&response_buf);
  return jsoncons::json::parse(s);
}

void Connection::send_requests(const std::vector<jsoncons::json>& msgs) {
  jsoncons::output_format format;
  format.escape_all_non_ascii(true);
  boost::asio::streambuf request_buf;
  std::ostream s(&request_buf);
  for (const auto& m : msgs) {
    m.to_stream(s, format);
    s << std::endl;
  }
  socket.send(request_buf.data());
}

}  // namespace utils