require 'json'
require 'socket'

server_host = ARGV[0]
server_port = ARGV[1]
bot_name = ARGV[2]
bot_key = ARGV[3]

puts "I'm #{bot_name} and connect to #{server_host}:#{server_port}"

class NoobBot
  def initialize(server_host, server_port, bot_name, bot_key)
    tcp = TCPSocket.open(server_host, server_port)
    play(bot_name, bot_key, tcp)
  end

  private

  def play(bot_name, bot_key, tcp)
    tcp.puts join_message(bot_name, bot_key)
    react_to_messages_from_server tcp
  end

  def react_to_messages_from_server(tcp)
    while json = tcp.gets
      message = JSON.parse(json)
      msgType = message['msgType']
      msgData = message['data']
      case msgType
        when 'carPositions'
          tcp.puts throttle_message(0.5)
        else
          case msgType
            when 'join'
              puts 'Joined'
            when 'gameStart'
              puts 'Race started'
            when 'crash'
              puts 'Someone crashed'
            when 'gameEnd'
              puts 'Race ended'
            when 'error'
              puts "ERROR: #{msgData}"
          end
          puts "Got #{msgType}"
          tcp.puts ping_message
      end
    end
  end

  def join_message(bot_name, bot_key)
    make_msg("join", {:name => bot_name, :key => bot_key})
  end

  def throttle_message(throttle)
    make_msg("throttle", throttle)
  end

  def ping_message
    make_msg("ping", {})
  end

  def make_msg(msgType, data)
    JSON.generate({:msgType => msgType, :data => data})
  end
end

NoobBot.new(server_host, server_port, bot_name, bot_key)
