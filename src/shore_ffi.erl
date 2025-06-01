-module(shore_ffi).
-export([to_continue/1, to_handle_msg/1, ssh_connection_send/3, to_ssh_secret/1]).

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

ssh_connection_send(Pid, Id, Data) ->
  case ssh_connection:send(Pid, Id, Data) of
    ok -> {ok, nil};
    Error -> Error
  end.

to_ssh_secret(Secret) ->
  case Secret of
    pubkey -> {public_key, nil};
    Password -> {user_password, unicode:characters_to_binary(Password)}
  end.
