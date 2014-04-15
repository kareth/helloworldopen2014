## Hello World Open (HWO) 2014 Bot Template

This is the HWO 2014 Bot Template. It contains simple racing bot implementations
for the Hello World Open 2014 competiion in different languages. You job is to
pick one of those and make it a winner.

Please refer to the online [Technical Specification](https://helloworldopen.com/techspec)
for most recent information. We won't be updating this document in your codebase :)

### Get started

1. Optionally, change your programming language by editing the `config` file.
   Then commit changes and push to master.

        vim config
        git commit -am "set language to Haskell" && push origin master

2. Code a winning bot. Our CI server will test your bot when you push to the
   master branch.

3. Try it against the test server (you'll get test server hostname and port
   from your team page)

        ./build
        ./run <host> <port>

Don't modify the scripts on the main level. These scripts ensure that all bots
respond to a common control interface.

### File Structure And Scripts

In this directory, there are some scripts that you can use to build and run your bot.
On the C.I system and in the competition, your bot will be run on the HWO servers
using these scripts.

- `./build` - build the bot, i.e. prepare for running
- `./run <host> <port> [<botname>]` - run the bot synchronously.

In subdirectories, there are bot implementations in different programming languages. The
`config` file determines which implementation is used. Each language folder contains two
scripts:

- `java/build` builds the Java bot
- `java/run <host> <port> <botname> <botkey>` runs the Java bot

### Logging

The C.I system will capture the standard output of your bot so you can log relevant events
by just writing to stdout. Please don't log all incoming and outgoing messages though, as
that would generate too much data. We'll probably limit the maximum number of lines captured.

### External dependencies

To ensure fast build/run times, the bots are built offline on the HWO servers. This means
that the build process cannot download external dependencies from the Internet. For each
bot template we've included the necessary dependencies (sockets, JSON) either on the
HWO server platform or into this codebase. If you need anything else, you'll need to find a way to
include the extra dependencies into your codebase.

The C.I system will build and run your bot regularly to ensure that it is compatible with
the build process on the HWO servers.

### Configuration

Language, botkey and name are configured in [config](config) file.

### Bot Protocol

See the [Technical specification](https://helloworldopen.com/techspec) online.
