-module(parsexml_xpath).

-export([parse/1, find/2]).

parse(XML) ->
	unpack_ns(parsexml:parse(XML)).

unpack_ns({Tag, Headers, Children}) ->
	{NS,CleanTag} = case binary:split(Tag, <<":">>) of
		[T1,T2] -> {T1,T2};
		[T1] -> {undefined,T1}
	end,
	{{NS,CleanTag}, [unpack_header(H) || H <- Headers], [unpack_ns(Ch) || Ch <- Children]};

unpack_ns(Bin) when is_binary(Bin) ->
	Bin.

unpack_header({K,V}) ->
	{NS, CleanK} = case binary:split(K, <<":">>) of
		[T1,T2] -> {T1,T2};
		[T1] -> {undefined,T1}
	end,
	{{NS,CleanK},V}.




-record(tag, {
	name,
	number,
	attr
}).

find(undefined, _) ->
	undefined;

find(XML, Path) when is_binary(XML) ->
	find(parse(XML), Path);

find(XML, Path) ->
	Segments = lists:map(fun(Segment) ->
		{match, [Name,Number,Attr]} = re:run(Segment, "^(?<tag>[^\\[@]+)(\\[(?<number>\\d+|\\*)\\])?(\\@(?<attr>.+))?$", [{capture,[tag,number,attr],binary}]),
		#tag{
			name = Name,
			number = case Number of <<>> -> undefined; <<"*">> -> all; _ -> binary_to_integer(Number) end,
			attr = case Attr of <<>> -> undefined; _ -> Attr end
		}
	end, binary:split(iolist_to_binary(Path), <<"/">>, [global])),
	lookup_path(XML, Segments).

lookup_path(XML, []) -> XML;
lookup_path({{_NS, Tag}, Attrs, _}, [#tag{name = Tag, attr = Attr}]) when Attr =/= undefined -> 
	case [V || {{_,K},V} <- Attrs, K == Attr] of
		[Value|_] -> Value;
		[] -> undefined
	end;
lookup_path({{_NS, Tag}, _, [Body]}, [#tag{name = Tag, attr = undefined}]) -> 
	Body;
lookup_path({{_NS, Tag}, _, Body}, [#tag{name = Tag}|Segments]) -> lookup_path(Body, Segments);
lookup_path({{_NS, _Tag1}, _, _}, [#tag{}|_]) -> undefined;
lookup_path(List, [#tag{name = Tag, number = N}|_] = Segments) when is_list(List) ->
	Children = [T || {{_NS,TagName}, _, _} = T <- List, TagName == Tag],

	case Children of
    _ when N == all ->
      [lookup_path(XML, Segments) || XML <- Children];
		_ when N =/= undefined andalso N >= 0 andalso N < length(Children) ->
			XML = lists:nth(N+1,Children),
			lookup_path(XML, Segments);
		_ when N =/= undefined ->
			undefined;
		[XML|_] ->
			lookup_path(XML, Segments);
		[] ->
			undefined
	end.




% lookup_path(XML, []) -> XML;
% lookup_path({Tag, _, Body}, [Tag|Segments]) -> lookup_path(Body, Segments);
% lookup_path({_, _, _}, [_|_]) -> undefined;
% lookup_path(List, [Tag|_] = Segments) ->
% 	case lists:keyfind(Tag, 1, List) of
% 		{Tag, _, [Body]} when length(Segments) == 1 -> Body;
% 		{Tag, _, _} = XML -> lookup_path(XML, Segments);
% 		false ->
% 			case re:run(Tag, "(.*)\\[(\\d+)\\]", [{capture,all_but_first,binary}]) of
% 				{match, [Tag1, Offset_]} ->
% 					Offset = binary_to_integer(Offset_),
% 					case [Body || {T,_,[Body]} <- List, T == Tag1] of
% 						Children when length(Children) > Offset ->
% 							lookup_path(lists:nth(Offset+1, Children), tl(Segments));
% 						_ ->
% 							undefined
% 					end;
% 				nomatch when length(Segments) == 1 ->
% 					case binary:split(Tag, <<"@">>) of
% 						[Tag1, Key] ->
% 							case lists:keyfind(Tag1, 1, List) of
% 								{Tag1, Attrs, _} -> proplists:get_value(Key, Attrs);
% 								false -> undefined
% 							end;
% 						_ ->
% 							undefined
% 					end;
% 				nomatch ->
% 					undefined
% 			end
% 	end.


