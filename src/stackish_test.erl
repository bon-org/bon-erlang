-module(stackish_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

start_trace() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(stackish, '_', []),
  dbg:p(all, c).

decode_test() ->
  {ok, Res} = stackish:decode(<<"[ \"child\" [ 200 '4:like' \"I\" \"hello\" things root">>),
  Ans = {root, [
    {things, ["hello", "I", <<"like">>, 200]},
    "child"
  ]},
  ?assertEqual(Ans, Res).