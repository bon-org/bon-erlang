-module(stackish).

-export([decode/1, encode/1]).

-record(node, {name, children}).

-compile(export_all).

%%%%%%%%%
%% API %%
%%%%%%%%%

decode(Data) when is_binary(Data) ->
  {ok, Stack} = tokenize(Data),
  io:format("Stack=~p~n", [Stack]),
  Res = parse(Stack),
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
-define(is_alphabet(B),
  (65 =< B andalso B =< (64 + 26)) or ((65 + 32) =< B andalso B =< (64 + 26 + 32))
).

%%%%%%%%%%%%%
% tokenizer %
%%%%%%%%%%%%%

tokenize(Data) ->
  tokenize(Data, [], nil, normal).

% rules: https://zedshaw.com/archive/stackish-an-xml-alternative/

tokenize(<<"[", T/binary>>, Stack, Acc, normal) ->
  tokenize(T, [mark | Stack], Acc, normal);

tokenize(<<" ", T/binary>>, Stack, Acc, normal) ->
  tokenize(T, Stack, Acc, normal);

%% parse string
tokenize(<<"\"", T/binary>>, Stack, nil, normal) ->
  tokenize(T, Stack, "", string);
tokenize(<<"\"", T/binary>>, Stack, Acc, string) ->
  io:format("finishing string, before=~p~n", [Acc]),
  Str = lists:reverse(Acc),
  io:format("after=~p~n", [Str]),
  tokenize(T, [Str | Stack], nil, normal);
tokenize(<<H, T/binary>>, Stack, Acc, string) ->
  tokenize(T, Stack, [H | Acc], string);

%% parse blob
tokenize(<<"'", T/binary>>, Stack, nil, normal) ->
  tokenize(T, Stack, 0, blob_num);
tokenize(<<H, T/binary>>, Stack, Len, blob_num) when ?is_digit(H) ->
  tokenize(T, Stack, Len * 10 + (H - 48), blob_num);
tokenize(<<":", T/binary>>, Stack, Len, blob_num) ->
  Tuple = {Len, []},
  tokenize(T, Stack, Tuple, blob);
tokenize(<<H, T/binary>>, Stack, {Len, Acc}, blob) when is_integer(Len) andalso Len > 0 ->
  Tuple = {Len - 1, [H | Acc]},
  tokenize(T, Stack, Tuple, blob);
tokenize(<<"'", T/binary>>, Stack, {0, Acc}, blob) ->
  Blob = erlang:list_to_binary(lists:reverse(Acc)),
  tokenize(T, [Blob | Stack], nil, normal);

%% parse word
tokenize(<<H, _/binary>> = Data, Stack, nil, normal) when ?is_alphabet(H) ->
  tokenize(Data, Stack, [], word);
tokenize(<<H, T/binary>>, Stack, Acc, word) when ?is_alphabet(H) ->
  tokenize(T, Stack, [H | Acc], word);
tokenize(Data, Stack, Acc, word) ->
  Word = {word, lists:reverse(Acc)},
  tokenize(Data, [Word | Stack], nil, normal);

%% parse number
tokenize(<<H, T/binary>>, Stack, nil, normal) when ?is_digit(H) ->
  tokenize(T, Stack, H - 48, number);
tokenize(<<H, T/binary>>, Stack, Acc, number) when ?is_digit(H) ->
  tokenize(T, Stack, Acc * 10 + (H - 48), number);
%%parse(<<H, _/binary>> = Data, Stack, Acc, number) when not (?is_digit(H)) ->
tokenize(Data, Stack, Acc, number) ->
  io:format("finishing number, value=~w~n", [Acc]),
  tokenize(Data, [Acc | Stack], nil, normal);

%% parse floating number

%% finished parsing state
tokenize(<<>>, Stack, nil, normal) ->
  {ok, Stack};

%% undefined parsing state
tokenize(Data, Stack, Acc, Mode) ->
  State = #{
    data => Data,
    stack => Stack,
    acc => Acc,
    mode => Mode
  },
  io:format("undefined parser state ~~>~p~n", [State]),
  {error, State}.


%%%%%%%%%%
% parser %
%%%%%%%%%%
parse(Stack) ->
  parse(Stack, nil, normal).

parse([{word, Name} | T0], nil, normal) ->
  {Children, [mark, T]} = lists:splitwith(fun(X) -> X =/= mark end, T0),
  Node = #node{name = Name, children = Children},
  parse([Node | T], nil, normal);

parse(Stack, Acc, Mode) ->
  State = #{
    stack => Stack,
    acc => Acc,
    mode => Mode
  },
  io:format("undefined transform state ~~>~p~n", [State]),
  {error, State}.