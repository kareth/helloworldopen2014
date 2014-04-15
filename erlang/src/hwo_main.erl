-module(hwo_main).
-export([main/0]).

main() ->
    [Host, PortStr, BotName, Key] = init:get_plain_arguments(),
    {Port, []} = string:to_integer(PortStr),
    io:fwrite("Starting bot ~p (connecting to ~s:~B)\n", [BotName, Host, Port]),
    hwo_lines:start(Host, Port),
    loop(BotName, Key).

%% Inbound message handling

onMessage(Pid, {struct, [{<<"msgType">>, <<"join">>}, 
                        {<<"data">>, _Data}]}) ->
    hwo_lines:send(Pid, pingMessage());

onMessage(Pid, {struct, [{<<"msgType">>, <<"carPositions">>}, 
                       {<<"data">>, _Data},
                       {<<"gameId">>, _GameId},
                       {<<"gameTick">>, _GameTick}]}) ->
    % io:fwrite("carPositions received, still speeding\n"),
    hwo_lines:send(Pid, throttleMessage(0.50));

onMessage(Pid, {struct, MessageProperties}) ->
    io:fwrite("Got ~p\n", [proplists:get_value(<<"msgType">>, MessageProperties, <<"Unknown">>)]),
    hwo_lines:send(Pid, pingMessage()).

%% Messages we send to server

joinMessage(BotName, Key) ->
    mochijson2:encode({struct, 
                       [{msgType, <<"join">>},
                        {data, {struct, 
                                [{name, list_to_binary(BotName)},
                                 {key, list_to_binary(Key)}]
                               }
                        }]}).

throttleMessage(Speed) ->
    mochijson2:encode({struct, 
                       [{msgType, <<"throttle">>},
                        {data, Speed}]}).

pingMessage() ->
    mochijson2:encode({struct, 
                       [{msgType, <<"ping">>}]}).

%% Main loop. hwo_lines is responsible for sending us parsed lines.

loop(BotName, Key) ->
    receive 
        {hwo_line_connected, Pid} ->
            hwo_lines:send(Pid, joinMessage(BotName, Key)),
            loop(BotName, Key);
        {hwo_line, Pid, Line} ->
            Msg = mochijson2:decode(Line),
            onMessage(Pid, Msg),
            loop(BotName, Key);
        hwo_line_closed ->
            io:fwrite("closed, exiting\n");
        {'EXIT', _, _} ->
            io:fwrite("IO process has died. A write error, likely. Must exit.\n");
        Msg ->
            io:fwrite("Error, exiting hwo_main: unknown msg: ~p\n", [Msg])
    end.

