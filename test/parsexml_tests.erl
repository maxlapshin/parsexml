-module(parsexml_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


parse1_test() ->
  ?assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
  parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\"><html>\n">>)).



parse2_test() ->
  {ok, Bin} = file:read_file("test1.xml"),
  ?assertEqual(
    {<<"p:program">>, [{<<"channel">>,<<"otv">>}, {<<"xmlns:p">>,<<"http://otv/ns">>}],  [
    ]},
  parsexml:parse(Bin)
  ).
