%% Binary Object Notation
%% A.K.A. Beeno Object Notation
%%
%% Based on stackish.
%%
%% Designed to enable loss-less data exchange between Erlang to Javascript.
%%
%% Loss-less means it will not mess up tuple and list, nor atom and hashmap key

-module(bon).

-export([encode/1, decode/1]).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


%%%%%%%%%
%% API %%
%%%%%%%%%

encode(Data) ->
  serialize(Data).

decode(Data) ->
  Data.


%%%%%%%%%%
%% Test %%
%%%%%%%%%%

data_test(Data) ->
  ?assertEqual(Data, decode(encode(Data))).

data_test() ->
  lists:map(fun data_test/1, [
    42,
    3.14,
    atom,
    "AB",
    [65, 0, 66],
    <<"text">>,
    {record, content},
    [{debug, true}, safe, speedup, {log, "file.log"}],
    #{user=>"name", pw=>"123"},
    #{12=>34, "user"=>name},
    #{log=>[#{time=> 123}, #{time=> 234}]}
  ]).


%%%%%%%%%%%%%%
%% Internal %%
%%%%%%%%%%%%%%

-define(NEED_SPACE, need_space).

is_string(X) ->
  io_lib:printable_unicode_list(X).

serialize(X) when is_integer(X) ->
  [$ , erlang:integer_to_binary(X), $ ];
serialize(X) when is_float(X) ->
  [$ , erlang:float_to_binary(X), $ ];

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
  List = erlang:tuple_to_list(Tuple),
  Children = lists:map(fun serialize/1, List),
  [$[, Children, $ , $t, $ ];

serialize(List) when is_list(List) ->
  case is_string(List) of
    true ->
      Str = lists:map(fun quote_string_char/1, List),
      [$", Str, $"];
    false ->
      Children = lists:map(fun serialize/1, List),
      [$[, Children, $ , $l, $ ]
  end.

quote_string_char($") -> [$\\, $"];
quote_string_char(C) -> C.

unquote_string_char([$\\, $"]) -> $";
unquote_string_char(C) -> C.

serialize_test(X) ->
  Res =
    binary_to_list(
      iolist_to_binary(
        serialize(X))),
  io:format("~p~n", [Res]).


-spec parse(InputStream, OutputStream) -> {RestInputStream, NewOutputStream} when
  InputStream :: iolist(),
  OutputStream :: [term()],
  RestInputStream :: iolist(),
  NewOutputStream :: [term()].

-define(is_digit(X), X >= $0 andalso X =< $9).
-define(is_small_cap(X), X >= $a andalso X =< $z).
-define(is_large_cap(X), X >= $A andalso X =< $Z).
-define(is_alphabet(X), ?is_small_cap(X) or ?is_large_cap(X)).
-define(is_word_body(X), ?is_digit(X) or ?is_alphabet(X)).

parse([], Acc) when is_list(Acc) ->
  {[], Acc};
parse(List, Acc) when is_list(List), is_list(Acc) ->
  {List, Acc}.

