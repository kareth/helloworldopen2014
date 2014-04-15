#!/usr/bin/php
<?php
require_once 'BasicBot.class.php';

if (count($argv) < 5) {
	die("Usage: bot host port botname botkey\n");
}
try {
	$bot = new BasicBot($argv[1], $argv[2], $argv[3], $argv[4]);
} catch (Exception $e) {
	die($e->getMessage() . "\n");
}
$bot->run();
?>


