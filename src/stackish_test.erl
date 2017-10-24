-module(stackish_test).

-export([]).

-include_lib("eunit/include/eunit.hrl").

decode_test() ->
  {ok, Res} = stackish:decode(<<"[ \"child\" [ 200 '4:like' \"I\" \"hello\" things root">>),
  Ans = {root, [
    {things, ["hello", "I", <<"like">>, 200]},
    "child"
  ]},
  ?assertEqual(Res, Ans).