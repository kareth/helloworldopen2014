-module(hwo_lines).
-export([start/2, 
         send/2, 
         init/3]).

%% Functions for reading and writing lines from a TCP socket.

%% API functions -------------------------------------------

% Connect to Host:Port and start listening for incoming lines.
%
% Once connection is formed caller will receive message 
% {hwo_line_connected, PID}.  
%
% For each incoming line the process calling start will receive 
% a message of format {hwo_line, PID, Line}. 
%
% When server closes connection message hwo_line_closed is sent.
%
start(Host, Port) ->
    spawn_link(?MODULE, init, [self(), Host, Port]).

% Send a message, you know the drill. Newline is appended for you. 
send(LineReaderPid, Message) ->
    LineReaderPid ! {send, Message},
    ok.

%% End of API functions ------------------------------------

%% Internal functions --------------------------------------

init(Pid, Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}]),
    Pid ! {hwo_line_connected, self()},
    loop(Pid, Socket).

loop(Pid, Socket) ->
    loop(Pid, Socket, []).

% UnfinishedLine keeps track of possible overflow data from last read.
loop(Pid, Socket, UnfinishedLine) -> 
    receive 
        {tcp, _S, Data} ->
            [Lines, UnfinishedLine2] = handleData([UnfinishedLine | Data]),
            handleLines(Lines, Pid),
            loop(Pid, Socket, UnfinishedLine2);
        {tcp_closed, _S} ->
            send_close(Pid);
        {send, Message} ->
            MsgBinary = list_to_binary(Message),
            NL = <<"\n">>,
            ok = gen_tcp:send(Socket, <<MsgBinary/binary,NL/binary>>),
            loop(Pid, Socket, UnfinishedLine);
        Msg ->
            io:fwrite("ERROR: unknown msg: ~p\n", [Msg]),
            send_close(Pid)
    end.

% Splits lines from read input. Returns both a lines array and separately last unfinished line.
% If Data ends with newline the unfinished line is empty.
handleData(Data) when is_list(Data) ->
    Lines = re:split(Data, "\n"),
    % if input ends with newline split leaves an empty list at last position
    case Lines of 
        [SingleUnfinishedLine] -> 
            [[], SingleUnfinishedLine];
        _ ->
            AllButLastLine = lists:sublist(Lines, 1, length(Lines) - 1),
            [AllButLastLine, lists:last(Lines)]
    end.

handleLines(Lines, Pid) ->
    lists:foreach(fun (L) ->
                          Pid ! {hwo_line, self(), L}
                  end, Lines).
                         
send_close(Pid) ->
     Pid ! hwo_line_closed.



