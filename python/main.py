import json
import socket
import sys


class NoobBot(object):

    def __init__(self, socket, name, key):
        self.socket = socket
        self.name = name
        self.key = key

    def msg(self, msg_type, data):
        self.send(json.dumps({"msgType": msg_type, "data": data}))

    def send(self, msg):
        self.socket.send(msg + "\n")

    def join(self):
        return self.msg("join", {"name": self.name,
                                 "key": self.key})

    def throttle(self, throttle):
        self.msg("throttle", throttle)

    def ping(self):
        self.msg("ping", {})

    def run(self):
        self.join()
        self.msg_loop()

    def on_join(self, data):
        print("Joined")
        self.ping()

    def on_game_start(self, data):
        print("Race started")
        self.ping()

    def on_car_positions(self, data):
        self.throttle(0.5)

    def on_crash(self, data):
        print("Someone crashed")
        self.ping()

    def on_game_end(self, data):
        print("Race ended")
        self.ping()

    def on_error(self, data):
        print("Error: {0}".format(data))
        self.ping()

    def msg_loop(self):
        msg_map = {
            'join': self.on_join,
            'gameStart': self.on_game_start,
            'carPositions': self.on_car_positions,
            'crash': self.on_crash,
            'gameEnd': self.on_game_end,
            'error': self.on_error,
        }
        socket_file = s.makefile()
        line = socket_file.readline()
        while line:
            msg = json.loads(line)
            msg_type, data = msg['msgType'], msg['data']
            if msg_type in msg_map:
                msg_map[msg_type](data)
            else:
                print("Got {0}".format(msg_type))
                self.ping()
            line = socket_file.readline()


if __name__ == "__main__":
    if len(sys.argv) != 5:
        print("Usage: ./run host port botname botkey")
    else:
        host, port, name, key = sys.argv[1:5]
        print("Connecting with parameters:")
        print("host={0}, port={1}, bot name={2}, key={3}".format(*sys.argv[1:5]))
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, int(port)))
        bot = NoobBot(s, name, key)
        bot.run()
