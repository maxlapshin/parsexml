-module(parsexml).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").



parse(Bin) when is_binary(Bin) ->
  ok.
