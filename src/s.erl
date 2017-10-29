-module(s).

% a rewrite of stackish from batch style into stream style

-export([decode/1, encode/1, encode/2]).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% external

-type stackish_options() :: [no_iolist|wrap_atom|existing_atom].
-define(default_options, [wrap_atom, existing_atom]).

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

%% internal

-record(word, {name}).
-record(string, {value}).

%%%%%%%%%
%% API %%
%%%%%%%%%

-spec decode(iolist()|binary(), stackish_options()) -> stackish_value().
decode(IOList, Opts) when is_list(IOList) ->
  decode(iolist_to_binary(IOList), Opts);
decode(Bins, Opts) when is_bitstring(Bins) ->
  throw(not_impl).

decode(Xs) ->
  decode(Xs, ?default_options).

-spec encode(stackish_value(), stackish_options()) -> iolist().
encode(Bins, Opts) ->
  throw(not_impl).

encode(Xs) ->
  encode(Xs, ?default_options).

%%%%%%%%%%%%%%
%% Internal %%
%%%%%%%%%%%%%%

- define(is_digit(B),
  48 =< B andalso B =< 57
).
-define(is_alphabet(B),
  (65 =< B andalso B =< (64 + 26)) or ((65 + 32) =< B andalso B =< (64 + 26 + 32))
).
