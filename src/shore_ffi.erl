-module(shore_ffi).

-export([hello_nif/0, setCbreak_nif/0, cmd/1, terminal/1, get_pos/0, printer/1]).

-nifs([hello_nif/0, setCbreak_nif/0]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("zig-out/lib/libshore", 0).

hello_nif() ->
    erlang:nif_error(nif_library_not_loaded).

setCbreak_nif() ->
    erlang:nif_error(nif_library_not_loaded).

cmd(Input) ->
  os:cmd(Input).

terminal(Code) ->
  Input = case Code of
    clear -> "\e[2J\e[H";
    top -> "\033[H";
    hidecursor -> "\e[?25l";
    showcursor -> "\e[?25h";
    {pos, X, Y} -> io_lib:format("\033[~w;~wH", [X, Y]);
    {up, I} -> io_lib:format("\033[~wA", [I]);
    {down, I} -> io_lib:format("\033[~wB", [I]);
    {left, I} -> io_lib:format("\033[~wD", [I]);
    {right, I} -> io_lib:format("\033[~wC", [I])
  end,
  io:format(Input).

get_pos() ->
  io:put_chars("\e[6n"),
  Pos = io:get_line(""),
  Pos.

printer(Input) ->
  io:format("\e[?25l\e[2J\e[H~ts", [Input]).
