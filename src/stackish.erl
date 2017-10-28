-module(stackish).

-export([decode/1, encode/1]).

-type stackish_value() :: true
| false
| stackish_string()
| stackish_number()
| stackish_map()
| stackish_array().

-type stackish_string() :: atom() | bitstring().
-type stackish_number() :: integer() | float().
-type stackish_map() :: #{stackish_map_key() => stackish_value()}.
-type stackish_array() :: [stackish_value()].

-type stackish_map_key() :: stackish_string()|stackish_number().


%%-record(node, {name, children}).
-record(word, {name}).
-record(string, {value}).


-compile(export_all).

%%%%%%%%%
%% API %%
%%%%%%%%%

-spec decode(bitstring()) -> stackish_value().

decode(Value) when is_binary(Value) ->
  case tokenize(Value) of
    {ok, Stack} -> parse(Stack);
    Res = {error, _Reason} -> Res
  end.

-spec encode(stackish_value()) -> bitstring().

encode(BitString) ->
  stringify(BitString).

%%%%%%%%%%%%%%
%% Internal %%
%%%%%%%%%%%%%%

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
  Str = lists:reverse(Acc),
  tokenize(T, [#string{value = Str} | Stack], nil, normal);
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
  Word = #word{name = lists:reverse(Acc)},
  tokenize(Data, [Word | Stack], nil, normal);

%% parse number
tokenize(<<H, T/binary>>, Stack, nil, normal) when ?is_digit(H) ->
  tokenize(T, Stack, H - 48, number);
tokenize(<<H, T/binary>>, Stack, Acc, number) when ?is_digit(H) ->
  tokenize(T, Stack, Acc * 10 + (H - 48), number);
%%parse(<<H, _/binary>> = Data, Stack, Acc, number) when not (?is_digit(H)) ->
tokenize(Data, Stack, Acc, number) ->
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
  io:format("undefined tokenizer state ~~>~p~n", [State]),
  {error, State}.


%%%%%%%%%%
% parser %
%%%%%%%%%%

parse(Tokens = [{word, _} | _]) ->
  parse(lists:reverse(Tokens));
parse(Tokens) ->
  try
    {ok, parse(Tokens, [])}
  catch
    {error, _Reason} = Res -> Res
  end.

parse([H = mark | T], Stack) ->
  parse(T, [H | Stack]);
parse([#string{value = Str} | T], Stack) ->
  parse(T, [Str | Stack]);
parse([H | T], Stack) when is_number(H) ->
  parse(T, [H | Stack]);
parse([H | T], Stack) when is_binary(H) ->
  parse(T, [H | Stack]);
parse([#word{name = Name} | T], Stack0) ->
  {Children, Stack1} = popUntilAndDrop(mark, Stack0),
  Node = #{Name => Children},
  parse(T, [Node | Stack1]);
parse([Map | T], Stack) when is_map(Map) ->
  parse(T, [Map | Stack]);
parse([], [Res]) ->
  Res.

popUntilAndDrop(Target, List) ->
  {L, R} = popUntilAndDrop(Target, List, []),
  {lists:reverse(L), R}.

-spec popUntilAndDrop(Target, List, Acc) -> {ReversedPoppedList, RestList} when
  Target :: T,
  List :: [T],
  Acc :: [T],
  ReversedPoppedList :: [T],
  RestList :: [T],
  T :: term().

popUntilAndDrop(_, [], Acc) ->
  {Acc, []};
popUntilAndDrop(Target, [Target | T], Acc) ->
  {Acc, T};
popUntilAndDrop(Target, [H | T], Acc) ->
  popUntilAndDrop(Target, T, [H | Acc]).


%%%%%%%%%%%%%%%
% stringifier %
%%%%%%%%%%%%%%%

-spec stringify(stackish_value()) -> iolist().

stringify(true) ->
  [<<"true">>];
stringify(false) ->
  [<<"false">>];

stringify({word, Name}) ->
  if
    is_atom(Name) ->
      Str = atom_to_list(Name),
      Bin = list_to_bitstring(Str),
      [Bin];
    is_integer(Name) ->
      Bin = integer_to_binary(Name),
      [Bin];
    is_float(Name) ->
      % warning, different language may have different precision, this may cause problem
      Bin = float_to_binary(Name),
      [Bin];
    is_list(Name) ->
      % assume it is string
      Bin = list_to_bitstring(Name),
      [Bin]
  end;

stringify({string, Str}) when is_list(Str) ->
  Bin = list_to_bitstring(Str),
  [$", Bin, $"];


stringify(Atom) when is_atom(Atom) ->
  stringify({string, atom_to_list(Atom)});

stringify(Binary) when is_binary(Binary) ->
  [Len] = stringify(byte_size(Binary)),
  [$', Len, $:, Binary, $'];

stringify(A) when is_integer(A) ->
  [integer_to_binary(A)];

stringify(A) when is_float(A) ->
  [float_to_binary(A)];

stringify(Map) when is_map(Map) ->
  Bs = lists:map(fun stackish:stringify/1, maps:keys(Map)),
  throw(not_impl);

stringify(A) when is_list(A) ->
  throw(not_impl);

stringify(_A) ->
  throw(not_impl).

-record(node, {value, name, parent, first_child, next_sibling}).
-define(has_sibling(Node), Node#node.next_sibling =/= undefined).
-define(has_child(Node), Node#node.first_child =/= undefined andalso is_record(Node#node.first_child, node)).

serialize(Node = #node{next_sibling = S}) when ?has_sibling(Node) ->
  Others = serialize(S),
  Self = serialize(Node#node{next_sibling = undefined}),
  [Self, $ , Others];

serialize(Node = #node{first_child = Child, name = Name}) when ?has_child(Node) ->
  Other = serialize(Child),
  Self = case Name of
           undefined -> $];
           S when is_list(S) -> list_to_bitstring(S);
           Bin when is_binary(Bin) -> Bin
         end,
  [Self, $ , Other];

serialize(#node{value = Bin}) when is_binary(Bin) ->
  Len = integer_to_binary(byte_size(Bin)),
  [$', Len, $:, Bin, $'];

serialize(#node{value = Str}) when is_list(Str) ->
  [$", list_to_bitstring(Str), $"];

serialize(#node{value = Num}) when is_number(Num) ->
  Str = if
          is_integer(Num) -> integer_to_binary(Num);
          is_float(Num) -> float_to_binary(Num)
        end,
  [Str];

serialize(X) ->
  throw(not_impl).

serialize_test() ->
  C2 = #node{value = <<"child">>},

  N4 = #node{value = 200},
  N3 = #node{value = <<"like">>, next_sibling = N4},
  N2 = #node{value = "I", next_sibling = N3},
  N1 = #node{value = "hello", next_sibling = N2},
  C1 = #node{name = <<"things">>, first_child = N1, next_sibling = C2},

  R = #node{name = <<"root">>, first_child = C1},

  Print =
    fun(Name, V0) ->
      Pat = iolist_to_binary([Name, " ~> "]),
      V = lists:flatten(V0),
      Res = lists:flatten([Pat, V]),
      io:format(Res ++ "~n")
    end,
  Print("N4", serialize(N4)),
  Print("N3", serialize(N3)),
  Print("N2", serialize(N2)),
  Print("N1", serialize(N1)),
  Print("C1", serialize(C1)),
  io:format("-------------------------~n").

% for debug
to_string(Xs) ->
  lists:reverse(to_string(Xs, [])).

to_string([], Acc) ->
  Acc;
to_string([H | T], Acc) when is_integer(H) ->
  to_string(T, [H | Acc]);
to_string([H | T], Acc) when is_binary(H) ->
  Bin = binary_to_list(H),
  Bs = lists:reverse(Bin),
  NewAcc = lists:flatten([Bs, Acc]),
  to_string(T, NewAcc).

debug() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(stackish, '_', []),
  dbg:p(all, c).

debug2() ->
  dbg:tpl(stackish, '_', []).
