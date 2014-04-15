package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"strconv"
)

func connect(host string, port int) (conn net.Conn, err error) {
	conn, err = net.Dial("tcp", fmt.Sprintf("%s:%d", host, port))
	return
}

func read_msg(reader *bufio.Reader) (msg interface{}, err error) {
	var line string
	line, err = reader.ReadString('\n')
	if err != nil {
		return
	}
	err = json.Unmarshal([]byte(line), &msg)
	if err != nil {
		return
	}
	return
}

func write_msg(writer *bufio.Writer, msgtype string, data interface{}) (err error) {
	m := make(map[string]interface{})
	m["msgType"] = msgtype
	m["data"] = data
	var payload []byte
	payload, err = json.Marshal(m)
	_, err = writer.Write([]byte(payload))
	if err != nil {
		return
	}
	_, err = writer.WriteString("\n")
	if err != nil {
		return
	}
	writer.Flush()
	return
}

func send_join(writer *bufio.Writer, name string, key string) (err error) {
	data := make(map[string]string)
	data["name"] = name
	data["key"] = key
	err = write_msg(writer, "join", data)
	return
}

func send_ping(writer *bufio.Writer) (err error) {
	err = write_msg(writer, "ping", make(map[string]string))
	return
}

func send_throttle(writer *bufio.Writer, throttle float32) (err error) {
	err = write_msg(writer, "throttle", throttle)
	return
}

func dispatch_msg(writer *bufio.Writer, msgtype string, data interface{}) (err error) {
	switch msgtype {
	case "join":
		log.Printf("Joined")
		send_ping(writer)
	case "gameStart":
		log.Printf("Game started")
		send_ping(writer)
	case "crash":
		log.Printf("Someone crashed")
		send_ping(writer)
	case "gameEnd":
		log.Printf("Game ended")
		// Exit is not strictly necessary?
		os.Exit(0)
	case "carPositions":
		send_throttle(writer, 0.5)
	case "error":
		log.Printf(fmt.Sprintf("Got error: %v", data))
		send_ping(writer)
	default:
		log.Printf("Got msg type: %s", msgtype)
		send_ping(writer)
	}
	return
}

func parse_and_dispatch_input(writer *bufio.Writer, input interface{}) (err error) {
	switch t := input.(type) {
	default:
		err = errors.New(fmt.Sprintf("Invalid message type: %T", t))
		return
	case map[string]interface{}:
		var msg map[string]interface{}
		var ok bool
		msg, ok = input.(map[string]interface{})
		if !ok {
			err = errors.New(fmt.Sprintf("Invalid message type: %v", msg))
			return
		}
		switch msg["data"].(type) {
		default:
			err = dispatch_msg(writer, msg["msgType"].(string), nil)
			if err != nil {
				return
			}
		case interface{}:
			err = dispatch_msg(writer, msg["msgType"].(string), msg["data"].(interface{}))
			if err != nil {
				return
			}
		}
	}
	return
}

func bot_loop(conn net.Conn, name string, key string) (err error) {
	reader := bufio.NewReader(conn)
	writer := bufio.NewWriter(conn)
	send_join(writer, name, key)
	for {
		input, err := read_msg(reader)
		if err != nil {
			log_and_exit(err)
		}
		err = parse_and_dispatch_input(writer, input)
		if err != nil {
			log_and_exit(err)
		}
	}
}

func parse_args() (host string, port int, name string, key string, err error) {
	args := os.Args[1:]
	if len(args) != 4 {
		return "", 0, "", "", errors.New("Usage: ./run host port botname botkey")
	}
	host = args[0]
	port, err = strconv.Atoi(args[1])
	if err != nil {
		return "", 0, "", "", errors.New(fmt.Sprintf("Could not parse port value to integer: %v\n", args[1]))
	}
	name = args[2]
	key = args[3]

	return
}

func log_and_exit(err error) {
	log.Fatal(err)
	os.Exit(1)
}

func main() {

	host, port, name, key, err := parse_args()

	if err != nil {
		log_and_exit(err)
	}

	fmt.Println("Connecting with parameters:")
	fmt.Printf("host=%v, port=%v, bot name=%v, key=%v\n", host, port, name, key)

	conn, err := connect(host, port)

	if err != nil {
		log_and_exit(err)
	}

	defer conn.Close()

	err = bot_loop(conn, name, key)
}
