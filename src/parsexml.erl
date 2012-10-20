-module(parsexml).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").



parse(Bin) when is_binary(Bin) ->
  [Tag] = parse(Bin, [tag], [[]]),
  Tag.


parse(<<"<?xml ", Bin/binary>>, [tag] = State, Acc) ->
  parse(Bin, [decl|State], Acc);

parse(<<"?>", Bin/binary>>, [decl|State], Acc) ->
  parse(Bin, State, Acc);

parse(<<_, Bin/binary>>, [decl|_] = State, Acc) ->
  parse(Bin, State, Acc);



parse(<<"</", Bin/binary>>, [tag|State], [Content, Attrs, Tag,Acc1|Acc]) ->
  Len = size(Tag),
  <<Tag:Len/binary, ">", Rest/binary>> = Bin,
  parse(Rest, State, [[{Tag,Attrs,lists:reverse(Content)}|Acc1]|Acc]);

parse(<<"<", Bin/binary>>, [tag|_]=State, Acc) ->
  parse(Bin, [tag_name|State], [<<>>|Acc]);

parse(<<"\n", Bin/binary>>, [tag|_] = State, Acc) ->
  parse(Bin, State, Acc);


%% Whole text parsing procedure
parse(<<_, _/binary>> = Bin, [tag|_]=State, [Acc1|Acc]) ->
  [Text, _] = binary:split(Bin, <<"<">>),
  Len = size(Text),
  <<Text:Len/binary, Rest/binary>> = Bin,
  parse(Rest, State, [[Text|Acc1]|Acc]);

% parse(<<"<", _/binary>> = Bin, [text|State], [Text,Acc1|Acc]) ->
%   parse(Bin, State, [[Text|Acc1]|Acc]);

% parse(<<C, Bin/binary>>, [text|_] = State, [Text|Acc]) ->
%   parse(Bin, State, [<<Text/binary,C>>|Acc]);

parse(<<" ", Bin/binary>>, [tag_name|State], Acc) ->
  parse(Bin, [tag_attr_list|State], [[]|Acc]);

parse(<<">", Bin/binary>>, [tag_attr_list|State], [Attrs, Tag|Acc]) ->
  parse(Bin, [tag|State], [[],lists:reverse(Attrs),Tag|Acc]);

parse(<<"/>", Bin/binary>>, [tag_attr_list,tag|State], [Attrs,Tag,Acc1|Acc]) ->
  parse(Bin, [tag|State], [[{Tag,lists:reverse(Attrs),[]}|Acc1]|Acc]);

parse(<<" ",Bin/binary>>, [tag_attr_list|_]=State, Acc) ->
  parse(Bin, State, Acc);

parse(<<C:1/binary,Bin/binary>>, [tag_attr_list|_]=State, Acc) ->
  parse(Bin, [tag_attr|State], [C|Acc]);

parse(<<"=", Quote:1/binary ,Bin/binary>>, [tag_attr|State], [Key,List|Acc]) ->
  [Value, Rest] = binary:split(Bin, Quote),
  parse(Rest, State, [[{Key,Value}|List]|Acc]);
  % parse(Bin, [tag_attr_value|State], [<<>>|Acc]);

parse(<<C,Bin/binary>>, [tag_attr|_] = State, [Key|Acc]) ->
  parse(Bin, State, [<<Key/binary, C>>|Acc]);

% parse(<<"\"", Bin/binary>>, [tag_attr_value|State], [Value, Key, List|Acc]) ->
%   parse(Bin, State, [[{Key,Value}|List]|Acc]);

% parse(<<C, Bin/binary>>, [tag_attr_value|_]=State, [Value|Acc]) ->
%   parse(Bin, State, [<<Value/binary,C>>|Acc]);

parse(<<"/>", Bin/binary>>, [tag_name,tag|State], [Tag,Acc1|Acc]) ->
  parse(Bin, [tag|State], [[{Tag,[],[]}|Acc1]|Acc]);

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

