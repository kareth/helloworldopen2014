local socket = require "socket"
local json = require "json"

local DEBUG = false

local function LOGD(msg, params)
   if DEBUG then
      print(string.format(msg, params))
   end
end

local NoobBot = {}
NoobBot.__index = NoobBot

function NoobBot.create(conn, name, key)
   local bot = {}
   setmetatable(bot, NoobBot)
   bot.conn = conn
   bot.name = name
   bot.key = key
   return bot
end

function NoobBot:msg(msgtype, data)
   return json.encode.encode({msgType = msgtype, data = data})
end

function NoobBot:send(msg)
   LOGD("Sending msg: %s", msg)
   self.conn:send(msg .. "\n")
end

function NoobBot:join()
   return self:msg("join", {name = self.name, key = self.key})
end

function NoobBot:throttle(throttle)
   return self:msg("throttle", throttle)
end

function NoobBot:ping()
   return self:msg("ping", {})
end

function NoobBot:run()
   self:send(self:join())
   self:msgloop()
end

function NoobBot:onjoin(data)
   print("Joined")
   self:send(self:ping())
end

function NoobBot:ongamestart(data)
   print("Race started")
   self:send(self:ping())
end

function NoobBot:oncarpositions(data)
   self:send(self:throttle(0.5))
end

function NoobBot:oncrash(data)
   print("Someone crashed")
   self:send(self:ping())
end

function NoobBot:ongameend(data)
   print("Race ended")
   self:send(self:ping())
end

function NoobBot:onerror(data)
   print("Error: " .. data)
   self:send(self:ping())
end

function NoobBot:msgloop()
   local line = self.conn:receive("*l")
   while line ~= nil or #len > 0 do
      LOGD("Got message: %s", line)
      local msg = json.decode.decode(line)
      if msg then
	 local msgtype = msg["msgType"]
	 local fn = NoobBot["on" .. string.lower(msgtype)]
	 if fn then
	    fn(self, msg["data"])
	 else
	    print("Got " .. msgtype)
	    self:send(self:ping())
	 end
      end
      line = self.conn:receive("*l")
   end
end

if #arg == 4 then
   local host, port, name, key = unpack(arg)
   print("Connecting with parameters:")
   print(string.format("host=%s, port=%s, bot name=%s, key=%s", unpack(arg)))
   local c = assert(socket.connect(host, port))
   bot = NoobBot.create(c, name, key)
   bot:run()
else
   print("Usage: ./run host port botname botkey")
end
