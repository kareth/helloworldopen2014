extern mod extra;

use std::io::buffered::BufferedStream;
use std::io::net::addrinfo::get_host_addresses;
use std::io::net::ip::{IpAddr, SocketAddr};
use std::io::net::tcp::TcpStream;
use std::os::args;

use extra::json;
use extra::json::{Json, ToJson};
use extra::treemap::TreeMap;

trait Protocol {
  fn msg_type(&self) -> ~str;
  fn json_data(&self) -> Json;
}

struct Msg {
  msgType: ~str,
  data: Json
}

impl Protocol for Msg {
  fn msg_type(&self) -> ~str { self.msgType.clone() }
  fn json_data(&self) -> Json { self.data.clone() }
}

struct JoinMsg {
  name: ~str,
  key: ~str
}

impl Protocol for JoinMsg {
  fn msg_type(&self) -> ~str { ~"join" }
  fn json_data(&self) -> Json {
    let mut m = TreeMap::new();
    m.insert(~"name", self.name.to_json());
    m.insert(~"key", self.key.to_json());
    return json::Object(~m);
  }
}

struct ThrottleMsg {
  value: f64
}

impl Protocol for ThrottleMsg {
  fn msg_type(&self) -> ~str { ~"throttle" }
  fn json_data(&self) -> Json { json::Number(self.value) }
}

fn write_msg<T: Protocol>(msg: &T, stream: &mut BufferedStream<TcpStream>) {
  let mut json = TreeMap::new();
  json.insert(~"msgType", msg.msg_type().to_json());
  json.insert(~"data", msg.json_data());

  write_json(&json::Object(~json), stream);
}

fn write_json(json: &Json, stream: &mut BufferedStream<TcpStream>) {
  json.to_writer(stream);
  stream.write_char('\n');
  stream.flush();
}

fn parse_msg(json: &~json::Object) -> Option<Msg> {
  match json.find(&~"msgType") {
    Some(&json::String(ref msgType)) => {
      let data = json.find(&~"data").unwrap_or(&json::Null);
      Some(Msg {
        msgType: msgType.clone(),
        data: data.clone()
      })
    }
    _ => None
  }
}

fn handle_msg(msg: ~Msg, stream: &mut BufferedStream<TcpStream>) {
  match msg.msgType {
    ~"carPositions" =>
      write_msg(&ThrottleMsg {
        value: 0.5
      }, stream),
    _ => {
      match msg.msgType {
        ~"join" => println("Joined"),
        ~"gameInit" => println("Race init"),
        ~"raceEnd" => println("Race end"),
        ~"raceStart" => println("Race start"),
        _ => println!("Got {:s}", msg.msgType)
      }
      write_msg(&Msg {
        msgType: ~"ping",
        data: json::Null
      }, stream);
    }
  }
}

fn start(config: Config) {
  let Config { server, name, key } = config;

  println!("Attempting to connect to {:s}", server.to_str());
  let mut stream = BufferedStream::new(
    TcpStream::connect(server).expect("Failed to connect"));
  println("Connected");

  write_msg(&JoinMsg {
    name: name,
    key: key
  }, &mut stream);

  loop {
    match stream.read_line() {
      None => break,
      Some(line) => match json::from_str(line) {
        Ok(json::Object(ref v)) => {
          match parse_msg(v) {
            None => println("Invalid JSON data"),
            Some(msg) => handle_msg(~msg, &mut stream)
          }
        },
        Ok(_) => println("Invalid JSON data: expected an object"),
        Err(msg) => println(msg.to_str())
      }
    }
  }
  println("Disconnected")
}

struct Config {
  server: SocketAddr,
  name: ~str,
  key: ~str
}

fn resolve_first_ip(host: &str) -> Option<IpAddr> {
  match get_host_addresses(host) {
    Some([ip, ..]) => Some(ip),
    _ => None
  }
}

fn read_config() -> Option<Config> {
  let args = args();
  match args {
    [_, host, port_str, name, key] => {
      let ip = resolve_first_ip(host).expect("Could not resolve host");
      let port = from_str::<u16>(port_str).expect("Invalid port number");
      return Some(Config {
        server: SocketAddr { ip: ip, port: port },
        name: name,
        key: key
      });
    },
    _ => None
  }
}

fn main() {
  match read_config() {
    None => println("Usage: ./run <host> <port> <botname> <botkey>"),
    Some(config) => start(config)
  }
}
