-module(shore_ffi).

-export([hello_nif/0]).

-nifs([hello_nif/0]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("zig-out/lib/libshore", 0).

hello_nif() ->
    erlang:nif_error(nif_library_not_loaded).
