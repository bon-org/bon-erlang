-module(stackish).

-export([decode/1, encode/1]).

-compile(export_all).


%%%%%%%%%
%% API %%
%%%%%%%%%

decode(Data) when is_binary(Data) ->
  {ok, Stack} = parse(Data),
  io:format("Stack=~p~n", [Stack]),
  {error, {not_impl, [Stack]}}.

encode(_Data) -> not_impl.


%%%%%%%%%%%%%%
%% Internal %%
%%%%%%%%%%%%%%

%%decode(<<>>, Acc) -> Acc;
%%decode(<<H, T/binary>>, []) -> not_impl.

-define(is_digit(B),
  48 =< B andalso B =< 57
).
-define(is_alphabet(B),
  (65 =< B andalso B =< (64 + 26)) or ((65 + 32) =< B andalso B =< (64 + 26 + 32))
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
  parse(T, [Str | Stack], nil, normal);
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
parse(<<H, _/binary>> = Data, Stack, nil, normal) when ?is_alphabet(H) ->
  parse(Data, Stack, [], word);
parse(<<H, T/binary>>, Stack, Acc, word) when ?is_alphabet(H) ->
  parse(T, Stack, [H | Acc], word);
parse(Data, Stack, Acc, word) ->
  Word = {word, lists:reverse(Acc)},
  parse(Data, [Word | Stack], nil, normal);

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

%% finished parsing state
parse(<<>>, Stack, nil, normal) ->
  {ok, Stack};

%% undefined parsing state
parse(Data, Stack, Acc, Mode) ->
  State = #{
    data => Data,
    stack => Stack,
    acc => Acc,
    mode => Mode
  },
  io:format("undefined parser state ~~>~p~n", [State]),
  {error, State}.


%%%%%%%%%%%%
% from_ast %
%%%%%%%%%%%%
from_ast([{word, _} | _] = Ast) ->
  from_ast(lists:reverse(Ast));
from_ast([A | T]) ->
  throw(error(implementing)).
