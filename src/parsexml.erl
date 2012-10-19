-module(parsexml).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").



parse(Bin) when is_binary(Bin) ->
  [Tag] = parse(Bin, [decl], [[]]),
  Tag.


parse(<<"<?xml ", Bin/binary>>, [decl], Acc) ->
  parse(Bin, [decl_close], Acc);

parse(<<"?>", Bin/binary>>, [decl_close], Acc) ->
  parse(Bin, [tag], Acc);

parse(<<_, Bin/binary>>, [decl_close], Acc) ->
  parse(Bin, [decl_close], Acc);




parse(<<"</", Bin/binary>>, [tag|State], [Content, Attrs, Tag,Acc1|Acc]) ->
  Len = size(Tag),
  <<Tag:Len/binary, ">", Rest/binary>> = Bin,
  parse(Rest, State, [[{Tag,Attrs,lists:reverse(Content)}|Acc1]|Acc]);

parse(<<"<", Bin/binary>>, [tag|State], Acc) ->
  parse(Bin, [tag_name,tag|State], [<<>>|Acc]);

parse(<<" ", Bin/binary>>, [tag_name|State], Acc) ->
  parse(Bin, [tag_attr_list|State], Acc);

parse(<<">", Bin/binary>>, [tag_name|State], Acc) ->
  parse(Bin, [tag|State], [[],[]|Acc]);

parse(<<C, Bin/binary>>, [tag_name|_] = State, [Tag|Acc]) ->
  parse(Bin, State, [<<Tag/binary, C>>|Acc]);



parse(<<>>, [tag], [Tag]) ->
  Tag;

parse(<<" ", Bin/binary>>, State, Acc) ->
  parse(Bin, State, Acc);

parse(<<"\n", Bin/binary>>, State, Acc) ->
  parse(Bin, State, Acc).

