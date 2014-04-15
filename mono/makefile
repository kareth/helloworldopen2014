.PHONY: clean build

all: build

clean:
	rm -rf bot.exe

build: bot.exe

bot.exe: bot.cs
	gmcs bot.cs -r:Newtonsoft.Json.dll