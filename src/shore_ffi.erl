-module(shore_ffi).
-export([thing/1, rawish/0, rawport/0, rawport2/0, leader/0, to_continue/1, to_handle_msg/1]).

thing(MyFun) ->
  receive
    {ssh_cm, Channel, {data, _, Data}} ->
        io:format("Received: ~p~n", [Data]),
        % Echo back
        Channel ! {ssh_cm, Channel, {data, 0, <<"You said: ", Data/binary>>}},
        loop();

    {ssh_cm, Channel, eof} ->
        Channel ! {ssh_cm, Channel, eof},
        Channel ! {ssh_cm, Channel, close},
        ok;

    {ssh_cm, _Channel, {signal, _, _Signal}} ->
        %% handle signals if needed
        loop();

    {'EXIT', _, _} ->
        ok;

    Msg -> MyFun(Msg)
  end.


loop() ->
  io:format("hi").

rawish() ->
  io:setopts(standard_io, [binary, {echo, false}]).


rawport() ->
  Port = open_port({spawn, "/bin/sh"},[use_stdio, exit_status, binary, eof]),
  port_command(Port, <<"stty raw -echo\n">>).
  %open_port({spawn, "echo 'hi'"}, []).


rawport2() ->
  open_port({spawn, "/tmp/shore_raw"},[]).


leader() -> 
  io:format("Group leader before: ~p~n", [group_leader()]).


%%
%% HEPLERS
%% 

to_continue(Result) ->
  case Result of
    {error, {Id, Reason}} -> {stop, Id, Reason};
    {ok, State} -> {ok, State}
  end.

% can't define 'EXIT' in gleam, need to type cast
to_handle_msg(Msg) ->
  case Msg of
    {'EXIT', Pid, Reason } -> {ssh_exit, Pid, Reason};
    Msg -> Msg
  end.
