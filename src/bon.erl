%% Binary Object Notation
%% A.K.A. Beeno Object Notation
%%
%% Based on stackish.
%%
%% Designed to enable loss-less data exchange between Erlang to Javascript.
%%
%% Loss-less means it will not mess up tuple and list, nor atom and hashmap key
%%
%% integer is represented in dec format, e.g. 42
%% floating number is represented in rational format (of integer), e.g. 314/100

-module(bon).

%% API
-export([encode/1, encode/2, decode/1, decode_all/1]).

%% Util
-export([fac/1]).

%% Testing
-include_lib("eunit/include/eunit.hrl").
-export([data_test/1, fac_test/1, serialize_test/1]).
-export([flatten_test/1]).
-export([debug/0, debug2/0]).

-define(list_to_atom(X), erlang:list_to_existing_atom(X)).

%%%%%%%%%
%% API %%
%%%%%%%%%

encode(Data, Opts) when is_list(Opts) ->
  IOList = serialize(Data),
  case Opts of
    [] ->
      IOList;
    [compact] ->
      shrink(IOList)
  end.

encode(Data) ->
  serialize(Data).

decode(IOList) when is_list(IOList) ->
  decode(iolist_to_binary(IOList));
decode(Bin) when is_binary(Bin) ->
  List = erlang:binary_to_list(Bin),
  {[], Res} = parse(List, []),
  Res.

decode_all(IOList) when is_list(IOList) ->
  decode(iolist_to_binary(IOList));
decode_all(Bin) when is_binary(Bin) ->
  List = erlang:binary_to_list(Bin),
  decode_all(List, []).

%%%%%%%%%%
%% Test %%
%%%%%%%%%%

data_test(Data) ->
  ?assertEqual(Data, decode(encode(Data))).

data_test() ->
  Res = lists:all(fun(X) -> data_test(X) == ok end, [
    42,
    -72,
    3.14,
    atom,
    "AB",
    [65, 0, 66],
    <<"text">>,
    {record, content},
    [{debug, true}, safe, speedup, {log, "file.log"}],
    #{user=>"name", pw=>"123"},
    #{12=>34, "user"=>name},
    #{log=>[#{time=> 123}, #{time=> 234}]},
    sets:from_list([42, 3.14, "str"])
  ]),
  case Res of
    true -> ok;
    X -> X
  end.


%%%%%%%%%%%%%%
%% Internal %%
%%%%%%%%%%%%%%

-spec decode_all(iolist(), [term()]) -> [term()].

decode_all(List, Acc) when is_list(List), is_list(Acc) ->
  case parse(List, []) of
    {[], Res} -> lists:reverse([Res | Acc]);
    {Next_List, Res} -> decode_all(Next_List, [Res | Acc])
  end.

is_string(X) ->
  io_lib:printable_unicode_list(X).

-define(WORD_TUPLE, "t").
-define(WORD_LIST, "l").
-define(WORD_MAP, "m").
-define(WORD_SET, "s").

%%%%%%%%%%%%%%
% serializer %
%%%%%%%%%%%%%%

serialize(X) when is_integer(X) ->
  [$ , erlang:integer_to_binary(X), $ ];
serialize(X) when is_float(X) ->
  Bin = case fac(X) of
          {A, 1} -> erlang:integer_to_binary(A);
          {A, B} -> [erlang:integer_to_binary(A), $/, erlang:integer_to_binary(B)]
        end,
  [$ , Bin, $ ];

serialize(X) when is_atom(X) ->
  Bin = list_to_binary(erlang:atom_to_list(X)),
  Size = erlang:byte_size(Bin),
  Bin_Size = erlang:integer_to_binary(Size),
  [$', $a, $:, Bin_Size, $:, Bin, $'];

serialize(Bin) when is_binary(Bin) ->
  Size = erlang:byte_size(Bin),
  Bin_Size = erlang:integer_to_binary(Size),
  [$', $b, $:, Bin_Size, $:, Bin, $'];

serialize(Tuple) when is_tuple(Tuple) ->
  case sets:is_set(Tuple) of
    true ->
      List = sets:to_list(Tuple),
      Children = serialize_list(List),
      [$[, Children, $ , ?WORD_SET, $ ];
    false ->
      List = erlang:tuple_to_list(Tuple),
      Children = serialize_list(List),
      [$[, Children, $ , ?WORD_TUPLE, $ ]
  end;

serialize(List) when is_list(List) ->
  case is_string(List) of
    true ->
      Str = lists:map(fun quote_string_char/1, List),
      [$", Str, $"];
    false ->
      Children = serialize_list(List),
      [$[, Children, $ , ?WORD_LIST, $ ]
  end;

serialize(Map) when is_map(Map) ->
  Children = maps:fold(
    fun(K, V, Acc) ->
      K_Bin = serialize(K),
      V_Bin = serialize(V),
      [V_Bin, K_Bin | Acc]
    end, [], Map),
  [$[, Children, $ , ?WORD_MAP, $ ].

serialize_list(List) when is_list(List) ->
  lists:foldl(
    fun(X, Acc) ->
      Bin = serialize(X),
      [Bin | Acc]
    end, [], List).

quote_string_char($") -> [$\\, $"];
quote_string_char(C) -> C.

serialize_test(X) ->
  Res =
    binary_to_list(
      iolist_to_binary(
        serialize(X))),
  io:format("~p~n", [Res]).


%%%%%%%%%%
% parser %
%%%%%%%%%%

-record(word, {name}).

-define(is_digit(X), (X >= $0 andalso X =< $9)).
-define(is_small_cap(X), (X >= $a andalso X =< $z)).
-define(is_large_cap(X), (X >= $A andalso X =< $Z)).
-define(is_alphabet(X), (?is_small_cap(X) or ?is_large_cap(X))).
-define(is_word_body(X), (?is_digit(X) or ?is_alphabet(X))).

-spec parse(InputStream, OutputStream) -> {RestInputStream, NewOutputStream} when
  InputStream :: iolist(),
  OutputStream :: [term()],
  RestInputStream :: iolist(),
  NewOutputStream :: [term()].

%% finish
parse([], [Acc]) ->
  {[], Acc};

%% skip spaces
parse([$  | T], Acc) ->
  parse(T, Acc);

%% number
parse([H | T], Acc) when ?is_digit(H) ->
  {Num, Tail} = parse_number(T, H - $0, 1),
  parse(Tail, [Num | Acc]);
parse([$-, H | T], Acc) when ?is_digit(H) ->
  {Num, Tail} = parse_number(T, H - $0, 1),
  parse(Tail, [-Num | Acc]);

%% atom
parse([$', $a, $:, H | T0], Acc) when ?is_digit(H) ->
  {Size, [$: | T1]} = parse_number(T0, H - $0, 2),
  {List, [$' | T2]} = lists:split(Size, T1),
  Atom = ?list_to_atom(List),
  parse(T2, [Atom | Acc]);

%% binary
parse([$', $b, $:, H | T0], Acc) when ?is_digit(H) ->
  {Size, [$: | T1]} = parse_number(T0, H - $0, 2),
  {List, [$' | T2]} = lists:split(Size, T1),
  Bin = list_to_binary(List),
  parse(T2, [Bin | Acc]);

%% group: tuple, list and map
parse([$[ | T0], Acc) ->
  {T1, [#word{name = Name} | Children]} = parse(T0, []),
  Res = case Name of
          ?WORD_TUPLE -> erlang:list_to_tuple(Children);
          ?WORD_LIST -> Children;
          ?WORD_MAP -> list_to_map(Children, #{});
          ?WORD_SET -> sets:from_list(Children)
        end,
  parse(T1, [Res | Acc]);

parse([H | T0], Acc) when ?is_alphabet(H) ->
  {Name, T1} = parse_word(T0, [H]),
  Word = #word{name = Name},
  {T1, [Word | Acc]};

%% string
parse([$" | T0], Acc) ->
  {Str, T1} = parse_string(T0, ""),
  parse(T1, [Str | Acc]).


%%%%%%%%%%%%%%%%%%%%%%
%% Helper Functions %%
%%%%%%%%%%%%%%%%%%%%%%

-spec parse_string(list(), [char()]) -> {[char()], list()}.

parse_string([], Acc) ->
  {lists:reverse(Acc), []};
parse_string([$\\, $" | T], Acc) ->
  parse_string(T, [$" | Acc]);
parse_string([$" | T], Acc) ->
  {lists:reverse(Acc), T};
parse_string([H | T], Acc) ->
  parse_string(T, [H | Acc]).

-spec parse_word(list(), [char()]) -> {[char()], list()}.

parse_word([H | T], Acc) when ?is_word_body(H) ->
  parse_word(T, [H | Acc]);
parse_word(Tail, Acc) when is_list(Tail) ->
  Name = lists:reverse(Acc),
  {Name, Tail}.

-spec parse_number(list(), integer(), 1|2) -> {number(), list()}.

parse_number([H | T], Acc, Count) when ?is_digit(H) ->
  parse_number(T, Acc * 10 + (H - $0), Count);
parse_number([$/, H | T0], Acc, 1) when ?is_digit(H) ->
  {Q, T1} = parse_number(T0, H - $0, 2),
  {Acc / Q, T1};
parse_number(List, Acc, Count) when is_list(List), is_number(Acc), (Count == 1) or (Count == 2) ->
  {Acc, List}.

-spec fac(float()|integer()) -> {integer(), integer()}.

fac(1) -> {1, 1};
fac(F) when F < 1 ->
  {A, B} = fac(1 / F),
  fac(B, A);
fac(F) ->
  {A, B} = fac_power_up(F, 1),
  fac(A, B).

fac_test(X) ->
  {A, B} = fac(X),
  Diff = abs(X - A / B),
  Diff / X.

fac(A, B) ->
  D = gcd(A, B),
  {round(A / D), round(B / D)}.

fac_power_up(F, Acc) when round(F) == F -> {round(F), Acc};
fac_power_up(F, Acc) -> fac_power_up(F * 10, Acc * 10).

gcd(0, X) -> X;
gcd(X, 0) -> X;
gcd(A, B) -> gcd(B, A rem B).

list_to_map([], Acc) when is_map(Acc) ->
  Acc;
list_to_map([K, V | T], Acc) ->
  list_to_map(T, maps:put(K, V, Acc)).

shrink(Xs) when is_list(Xs) ->
  shrink(Xs, []).

shrink([], Acc) ->
  Acc;
shrink([H | T], Acc) when is_list(H) ->
  shrink(H, shrink(T, Acc));
shrink([$  | T], [$  | Acc]) ->
  shrink(T, [$  | Acc]);
shrink([H | T], Acc) ->
  [H | shrink(T, Acc)].

flatten_test(X) ->
  shrink(X).

debug() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(?MODULE, '_', []),
  dbg:p(all, c).

debug2() ->
  dbg:tpl(?MODULE, '_', []).

%%phi() ->
%%  phi(1).
%%phi(X) ->
%%  Y = 1 + 1 / X,
%%  if X == Y -> X;
%%    true -> phi(Y)
%%  end.
