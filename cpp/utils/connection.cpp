#include "utils/connection.h"

#include "gflags/gflags.h"


DECLARE_string(race_id);
DEFINE_bool(dump_history, false, "");

namespace utils {

Connection::Connection(const std::string& host, const std::string& port)
  : socket(io_service) {

  tcp::resolver resolver(io_service);
  tcp::resolver::query query(host, port);
  boost::asio::connect(socket, resolver.resolve(query));

  socket.set_option(tcp::no_delay(true));

  response_buf.prepare(8192);
  history_ = jsoncons::json(jsoncons::json::an_array);
}

Connection::~Connection() {
  socket.close();

  if (FLAGS_dump_history) {
    std::ofstream file;
    file.open("bin/" + FLAGS_race_id + "/history.json");
    jsoncons::output_format format;
    history_.to_stream(file, format);
    file << std::endl;
    file.close();
  }
}

jsoncons::json Connection::receive_response(boost::system::error_code* error) {
  auto len = boost::asio::read_until(socket, response_buf, "\n", *error);
  if (*error) {
    return jsoncons::json();
  }
  auto buf = response_buf.data();
  std::string reply(boost::asio::buffers_begin(buf), boost::asio::buffers_begin(buf) + len);
  response_buf.consume(len);
  auto json_reply = jsoncons::json::parse_string(reply);
  if (FLAGS_dump_history) {
    history_.add(json_reply);
  }
  return json_reply;
}

void Connection::send_requests(const std::vector<jsoncons::json>& msgs) {
  jsoncons::output_format format;
  format.escape_all_non_ascii(true);
  boost::asio::streambuf request_buf;
  std::ostream s(&request_buf);
  for (const auto& m : msgs) {
    // std::cout << "Responded with: " << pretty_print(m) << std::endl;
    m.to_stream(s, format);
    s << std::endl;
    if (FLAGS_dump_history) {
      history_.add(m);
    }
  }
  socket.send(request_buf.data());
}

}  // namespace utils
