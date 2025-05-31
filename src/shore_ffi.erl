-module(shore_ffi).
-export([thing/1, rawish/0, rawport/0, rawport2/0, tty/0, leader/0, echo/1, ptyopts/0]).

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

tty() ->
  prim_tty:init_ssh(#{input => raw, output => raw}, {80, 24}, utf8).

echo(TTY) ->
  receive
    _M ->
      io:format("msg"),
      echo(TTY)
  end.

ptyopts() ->
    ssh_channel:info(pty_options, self()).
