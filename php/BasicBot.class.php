<?php
define('MAX_LINE_LENGTH', 1024 * 1024);

class BasicBot {
	protected $sock, $debug;

	function __construct($host, $port, $botname, $botkey, $debug = FALSE) {
		$this->debug = $debug;
		$this->connect($host, $port, $botkey);
		$this->write_msg('join', array(
			'name' => $botname,
			'key' => $botkey
		));
	}

	function __destruct() {
		if (isset($this->sock)) {
			socket_close($this->sock);
		}
	}

	protected function connect($host, $port, $botkey) {
		$this->sock = @ socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
		if ($this->sock === FALSE) {
			throw new Exception('socket: ' . socket_strerror(socket_last_error()));
		}
		if (@ !socket_connect($this->sock, $host, $port)) {
			throw new Exception($host . ': ' . $this->sockerror());
		}
	}

	protected function read_msg() {
		$line = @ socket_read($this->sock, MAX_LINE_LENGTH, PHP_NORMAL_READ);
		if ($line === FALSE) {
			$this->debug('** ' . $this->sockerror());
		} else {
			$this->debug('<= ' . rtrim($line));
		}
		return json_decode($line, TRUE);
	}

	protected function write_msg($msgtype, $data) {
		$str = json_encode(array('msgType' => $msgtype, 'data' => $data)) . "\n";
		$this->debug('=> ' . rtrim($str));
		if (@ socket_write($this->sock, $str) === FALSE) {
			throw new Exception('write: ' . $this->sockerror());
		}
	}
	
	protected function sockerror() {
		return socket_strerror(socket_last_error($this->sock));
	}
	
	protected function debug($msg) {
		if ($this->debug) {
			echo $msg, "\n";
		}
	}
	
	public function run() {
		while (!is_null($msg = $this->read_msg())) {
			switch ($msg['msgType']) {
				case 'carPositions':
					$this->write_msg('throttle', 0.5);
					break;
				case 'join':
				case 'yourCar':
				case 'gameInit':
				case 'gameStart':
				case 'crash':
				case 'spawn':
				case 'lapFinished':
				case 'dnf':
				case 'finish':
				default:
					$this->write_msg('ping', null);
			}
		}
	}
}
?>