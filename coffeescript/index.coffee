net        = require("net")
JSONStream = require('JSONStream')

serverHost = process.argv[2]
serverPort = process.argv[3]
botName = process.argv[4]
botKey = process.argv[5]

console.log("I'm", botName, "and connect to", serverHost + ":" + serverPort)

client = net.connect serverPort, serverHost, () ->
  send({ msgType: "join", data: { name: botName, key: botKey }})

send = (json) ->
  client.write JSON.stringify(json)
  client.write '\n'

jsonStream = client.pipe(JSONStream.parse())

jsonStream.on 'data', (data) ->
  if data.msgType == 'carPositions'
    send {msgType: "throttle", data: 0.5}
  else
    if data.msgType == 'join'
      console.log 'Joined'
    else if data.msgType == 'gameStart'
      console.log 'Race started'
    else if data.msgType == 'gameEnd'
      console.log 'Race ended'
    send {msgType: "ping", data: {}}

jsonStream.on 'error', ->
  console.log "disconnected"
