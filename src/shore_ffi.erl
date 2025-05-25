-module(shore_ffi).
-export([thing/1]).

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
