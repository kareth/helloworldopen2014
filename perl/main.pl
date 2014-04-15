#!/usr/bin/perl
use strict;
use warnings;
use 5.010;
use JSON;
use IO::Socket::INET;

my ($server_host, $server_port, $bot_name, $bot_key) = @ARGV;

say "I'm $bot_name and connect to $server_host:$server_port using key $bot_key";

initialize($server_host, $server_port, $bot_name, $bot_key);

sub initialize {
  my ($server_host, $server_port, $bot_name, $bot_key) = @_;
  my $socket = new IO::Socket::INET(
    PeerHost => $server_host,
    PeerPort => $server_port,
    Proto => "tcp",
  ) or die "Failed to open TCP socket: $!\n";

  play($bot_name, $bot_key, $socket);
}

sub play {
  my ($bot_name, $bot_key, $socket) = @_;
  print $socket join_message($bot_name, $bot_key);
  react($socket);
}


sub react {
  my ($socket) = @_;
  while (<$socket>) {
    my %json = %{decode_json $_};

    my $msg_type = $json{"msgType"};
    my $msg_data = $json{"data"};

    given ($msg_type) {
      when ("carPositions") {
        print $socket throttle_message(0.5);
      }
      default {
        given ($msg_type) {
          when ("join") {
            say "Joined";
          }
          when ("gameStart") {
            say "Race started";
          }
          when ("crash") {
            say "Someone crashed";
          }
          when ("gameEnd") {
            say "Race ended";
          }
          when ("error") {
            say "ERROR: $msg_data";
          }
          say "Got $msg_type";
          print $socket ping_message();
        }
      }
    }
  }
}

sub join_message {
  my ($bot_name, $bot_key) = @_;
  my %msg_hash = ("name"=>$bot_name, "key"=>$bot_key);
  make_msg("join", \%msg_hash);
}

sub throttle_message {
  my ($throttle) = @_;
  make_msg("throttle",$throttle);
}

sub ping_message {
  make_msg("ping", {});
}

sub make_msg {
  my ($msg_type, $data) = @_;
  my %msg_hash = ('msgType'=>$msg_type, 'data'=>$data);
  encode_json (\%msg_hash) . "\n";
}
