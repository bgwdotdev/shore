-module(shore_ffi).

-export([hello_nif/0, setCbreak_nif/0, cmd/1, get_pos/0]).

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

get_pos() ->
  io:put_chars("\e[6n"),
  Pos = io:get_line(""),
  Pos.

