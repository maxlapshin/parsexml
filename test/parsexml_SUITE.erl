-module(parsexml_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
  [{group, parsexml}].

groups() ->
  [{parsexml, [parallel], [
    parse1,
    parse2,
    parse3
  ]}].

parse1(_) -> 
  ?assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>))
  ,?assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"\n\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>))
  ,?assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"\n<html></html>\n">>))
  ,?assertEqual({<<"html">>, [], []},
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html ></html>\n">>))
  ,?assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\"></html>\n">>))
  ,?assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\" ></html>\n">>))
  ,?assertEqual({<<"html">>, [{<<"xmlns">>,<<"w3c">>}], []}, 
    parsexml:parse(<<"<html xmlns='w3c' />\n">>))
  ,?assertEqual({<<"html">>, [], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html/>\n">>))
  ,?assertEqual({<<"html">>, [], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html />\n">>))
  ,?assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\"/>\n">>))
  ,?assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\n">>))
  ,?assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []}, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\r\n">>))
  ,?assertEqual({<<"html">>, [{<<"k">>,<<"v">>}], []},
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html k=\"v\" />\n">>))
  ,
  ok.

parse2(_) ->
  {ok, Bin} = file:read_file(code:lib_dir(parsexml,test)++"/test1.xml"),
  {<<"p:program">>, [{<<"channel">>,<<"otv">>}, {<<"xmlns:p">>,<<"http://otv/ns">>}],  [
      {<<"p:day">>, [{<<"date">>,<<"2011-10-19">>}], [
        {<<"p:item">>, [{<<"id">>,<<"2877">>},{<<"href">>,<<"http://otv/15/">>},{<<"title">>,<<"New">>}],[]},
        {<<"p:item">>, [{<<"id">>,<<"2878">>},{<<"href">>,<<"http://otv/16/">>},
          {<<"title">>,<<"Chan &quot;Morning&quot; (0+)">>}],[
          _
        ]}
      ]}
  ]} = parsexml:parse(Bin),
  ok.


parse3(_) ->
  {ok, Bin} = file:read_file(code:lib_dir(parsexml,test)++"/test2.xml"),
  ?assertMatch(
    {<<"MPD">>, _,  [_]}, parsexml:parse(Bin)
  ).
