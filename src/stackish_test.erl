-module(stackish_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(Data,
  #{"root" => [
    #{"things"=>[
      "hello",
      "I",
      <<"like">>,
      200
    ]},
    "child"
  ]}).

-define(Raw,
  <<"[ \"child\" [ 200 '4:like' \"I\" \"hello\" things root">>
).

start_trace() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(stackish, '_', []),
  dbg:p(all, c).

decode_test() ->
  {ok, Res} = stackish:decode(?Raw),
  ?assertEqual(?Data, Res).

encode_test() ->
  ?assertEqual(<<"true">>, iolist_to_binary(stackish:stringify(true))),
  ?assertEqual(<<"false">>, iolist_to_binary(stackish:stringify(false))),
  ?assertEqual(<<"123">>, iolist_to_binary(stackish:stringify(123))),
  ?assertEqual(<<"3.14000000000000012434e+00">>, iolist_to_binary(stackish:stringify(3.14))),
  ?assertEqual(<<"'8:username'">>, iolist_to_binary(stackish:stringify(<<"username">>))),
  ?assertEqual(<<"\"text\"">>, iolist_to_binary(stackish:stringify({string, "text"}))),
  ?assertEqual(<<"atom">>, iolist_to_binary(stackish:stringify({word, atom}))),
  {ok, Res} = stackish:encode(?Data),
  ?assertEqual(?Raw, iolist_to_binary(Res)).

data_test(Data) ->
  io:format("Data=~p~n", [Data]),
  {ok, Bin} = stackish:encode(Data,[no_iolist]),
  io:format("Bin=~p~n", [Bin]),
  {ok, Res} = stackish:decode(Bin),
  ?assertEqual(Data, Res),
  io:format("passed on ~p~n", [Data]).

data_test() ->
  data_test(?Data),
  data_test(123),
  data_test("321"),
  data_test("text"),
  data_test(atom),
  data_test([1, 2, 3]),
  ok.
