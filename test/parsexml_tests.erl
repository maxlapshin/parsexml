-module(parsexml_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


parse_test_() -> 
  [
  ?_assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>))
  ,?_assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"\n<html></html>\n">>))
  ,?_assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html ></html>\n">>))
  ,?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\"></html>\n">>))
  ,?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\" ></html>\n">>))
  ,?_assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
    parsexml:parse(<<"<html xmlns='w3c' />\n">>))
  ,?_assertEqual({<<"html">>, [], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html/>\n">>))
  ,?_assertEqual({<<"html">>, [], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html />\n">>))
  ,?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\"/>\n">>))
  ,?_assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\n">>))
  ].

parse2_test() ->
  {ok, Bin} = file:read_file("../test/test1.xml"),
  ?assertEqual(
    {<<"p:program">>, [{<<"channel">>,<<"otv">>}, {<<"xmlns:p">>,<<"http://otv/ns">>}],  [
      {<<"p:day">>, [{<<"date">>,<<"2011-10-19">>}], [
        {<<"p:item">>, [{<<"id">>,<<"2877">>},{<<"href">>,<<"http://otv/15/">>},{<<"title">>,<<"New">>}],[]},
        {<<"p:item">>, [{<<"id">>,<<"2878">>},{<<"href">>,<<"http://otv/16/">>},
          {<<"title">>,<<"Chan &quot;Morning&quot; (0+)">>}],[
          <<"Morning &lt;chan&gt;,…">>
        ]}
      ]}
    ]},
  parsexml:parse(Bin)
  ).
