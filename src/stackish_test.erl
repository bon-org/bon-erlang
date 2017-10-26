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
