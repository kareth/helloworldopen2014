.PHONY: clean build

all: build

clean:
	rm -rf bot.exe

build: bot.exe

bot.exe: bot.fs sample.json
	fsharpc bot.fs -r:FSharp.Data.dll