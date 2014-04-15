var net = require("net");
var JSONStream = require('JSONStream');

var serverHost = process.argv[2];
var serverPort = process.argv[3];
var botName = process.argv[4];
var botKey = process.argv[5];

console.log("I'm", botName, "and connect to", serverHost + ":" + serverPort);

client = net.connect(serverPort, serverHost, function() {
  return send({
    msgType: "join",
    data: {
      name: botName,
      key: botKey
    }
  });
});

function send(json) {
  client.write(JSON.stringify(json));
  return client.write('\n');
};

jsonStream = client.pipe(JSONStream.parse());

jsonStream.on('data', function(data) {
  if (data.msgType === 'carPositions') {
    send({
      msgType: "throttle",
      data: 0.5
    });
  } else {
    if (data.msgType === 'join') {
      console.log('Joined')
    } else if (data.msgType === 'gameStart') {
      console.log('Race started');
    } else if (data.msgType === 'gameEnd') {
      console.log('Race ended');
    } 

    send({
      msgType: "ping",
      data: {}
    });
  }
});

jsonStream.on('error', function() {
  return console.log("disconnected");
});
