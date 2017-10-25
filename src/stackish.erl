-module(stackish).

-export([decode/1, encode/1]).

-compile(export_all).


%%%%%%%%%
%% API %%
%%%%%%%%%

decode(Data) ->
  Res = parse(Data),
  io:format("Res=~p~n", [Res]),
  {error, {not_impl, [Res]}}.

encode(_Data) -> not_impl.


%%%%%%%%%%%%%%
%% Internal %%
%%%%%%%%%%%%%%

%%decode(<<>>, Acc) -> Acc;
%%decode(<<H, T/binary>>, []) -> not_impl.

-define(is_digit(B),
  48 =< B andalso B =< 57
).

%%%%%%%%%%
% parser %
%%%%%%%%%%

parse(Data) ->
  parse(Data, [], nil, normal).

% rules: https://zedshaw.com/archive/stackish-an-xml-alternative/

parse(<<"[", T/binary>>, Stack, Acc, normal) ->
  parse(T, [mark | Stack], Acc, normal);

parse(<<" ", T/binary>>, Stack, Acc, normal) ->
  parse(T, Stack, Acc, normal);

%% parse string
parse(<<"\"", T/binary>>, Stack, nil, normal) ->
  parse(T, Stack, "", string);
parse(<<"\"", T/binary>>, Stack, Acc, string) ->
  io:format("finishing string, before=~p~n", [Acc]),
  Str = lists:reverse(Acc),
  io:format("after=~p~n", [Str]),
  parse(T, [Str | Stack], nil, string);
parse(<<H, T/binary>>, Stack, Acc, string) ->
  parse(T, Stack, [H | Acc], string);

%% parse blob
parse(<<"'", T/binary>>, Stack, nil, normal) ->
  parse(T, Stack, 0, blob_num);
parse(<<H, T/binary>>, Stack, Len, blob_num) when ?is_digit(H) ->
  parse(T, Stack, Len * 10 + (H - 48), blob_num);
parse(<<":", T/binary>>, Stack, Len, blob_num) ->
  Tuple = {Len, []},
  parse(T, Stack, Tuple, blob);
parse(<<H, T/binary>>, Stack, {Len, Acc}, blob) when is_integer(Len) andalso Len > 0 ->
  Tuple = {Len - 1, [H | Acc]},
  parse(T, Stack, Tuple, blob);
parse(<<"'", T/binary>>, Stack, {0, Acc}, blob) ->
  Blob = erlang:list_to_binary(lists:reverse(Acc)),
  parse(T, [Blob | Stack], nil, normal);

%% parse word

%% parse number
parse(<<H, T/binary>>, Stack, nil, normal) when ?is_digit(H) ->
  parse(T, Stack, H - 48, number);
parse(<<H, T/binary>>, Stack, Acc, number) when ?is_digit(H) ->
  parse(T, Stack, Acc * 10 + (H - 48), number);
%%parse(<<H, _/binary>> = Data, Stack, Acc, number) when not (?is_digit(H)) ->
parse(Data, Stack, Acc, number) ->
  io:format("finishing number, value=~w~n", [Acc]),
  parse(Data, [Acc | Stack], nil, normal);

%% parse floating number

parse(Data, Stack, Acc, Mode) ->
  io:format("undefined parser state, ~p~n", [#{
    data => Data,
    stack => Stack,
    acc => Acc,
    mode => Mode
  }]),
  not_impl.
