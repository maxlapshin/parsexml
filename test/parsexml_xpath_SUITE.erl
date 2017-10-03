-module(parsexml_xpath_SUITE).

-compile(export_all).


all() ->
	[{group, parse}].


groups() ->
	[{parse, [parallel], [
		parse,
		find_path1,
		parse_discovery_response,
		parse_profiles_response
	]}].



nvt_discovery() ->
<<"<s:Envelope xmlns:s=\"http://www.w3.org/2003/05/soap-envelope\" "
"xmlns:a=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"><s:Header>"
"<a:Action s:mustUnderstand=\"1\">http://schemas.xmlsoap.org/ws/2005/04/discovery/Probe"
"</a:Action><a:MessageID>uuid:6cc5627f-ebb6-423f-afe0-3f45456342ed</a:MessageID>"
"<a:ReplyTo><a:Address>http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous"
"</a:Address></a:ReplyTo><a:To s:mustUnderstand=\"1\">"
"urn:schemas-xmlsoap-org:ws:2005:04:discovery</a:To></s:Header><s:Body>"
"<Probe xmlns=\"http://schemas.xmlsoap.org/ws/2005/04/discovery\"><d:Types "
"xmlns:d=\"http://schemas.xmlsoap.org/ws/2005/04/discovery\" "
"xmlns:dp0=\"http://www.onvif.org/ver10/network/wsdl\">dp0:NetworkVideoTransmitter</d:Types>"
"</Probe></s:Body></s:Envelope>">>.

get_dns() ->
<<"<s:Envelope xmlns:s=\"http://www.w3.org/2003/05/soap-envelope\">"
"<s:Body xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
"xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">"
"<GetDNS xmlns=\"http://www.onvif.org/ver10/device/wsdl\"/>"
"</s:Body></s:Envelope>">>.


parse(_) ->
	{{<<"s">>,<<"Envelope">>}, _, [
		{{<<"s">>,<<"Header">>}, [], _},
		{{<<"s">>,<<"Body">>}, [], [
			{{undefined, <<"Probe">>}, _, [
				{{<<"d">>,<<"Types">>}, _, [<<"dp0:NetworkVideoTransmitter">>]}
			]}
		]}
	]} = parsexml_xpath:parse(nvt_discovery()),

	{{<<"s">>, <<"Envelope">>}, _, [
		{{<<"s">>, <<"Body">>}, [
			{{<<"xmlns">>,<<"xsi">>},<<"http://www.w3.org/2001/XMLSchema-instance">>},
			{{<<"xmlns">>,<<"xsd">>},<<"http://www.w3.org/2001/XMLSchema">>}
		], [
			{{undefined, <<"GetDNS">>}, _, []}
		]}
	]} = parsexml_xpath:parse(get_dns()),
	ok.
	


find_path1(_) ->
	<<"uuid:6cc5627f-ebb6-423f-afe0-3f45456342ed">> = parsexml_xpath:find(nvt_discovery(), <<"Envelope/Header/MessageID">>),
	undefined = parsexml_xpath:find(nvt_discovery(), <<"Header/MessageID">>),
	undefined = parsexml_xpath:find(nvt_discovery(), <<"Envelope/Header15/MessageID">>),
	undefined = parsexml_xpath:find(undefined, <<"Header/MessageID">>),
	% undefined = parsexml_xpath:find()
	ok.


parse_discovery_response(_) ->
	{ok, Bin} = file:read_file(filename:join(code:lib_dir(parsexml,test), "discovery.xml")),
	<<"uuid:3c0468df-9900-49de-a7fc-2abe7702456b">> = parsexml_xpath:find(Bin, <<"Envelope/Header/RelatesTo">>),
	ProbeMatch = parsexml_xpath:find(Bin, <<"Envelope/Body/ProbeMatches/ProbeMatch">>),
	<<"dp0:Device">> = parsexml_xpath:find(ProbeMatch, <<"Types">>),
	<<"http://192.168.0.123:80/onvif/device_service">> = parsexml_xpath:find(ProbeMatch, <<"XAddrs">>),
	ok.

parse_profiles_response(_) ->
	{ok, Bin} = file:read_file(filename:join(code:lib_dir(parsexml,test), "GetProfilesResponse.xml")),
	<<"MainStream">> = parsexml_xpath:find(Bin, <<"Envelope/Body/GetProfilesResponse/Profiles[0]@token">>),
	<<"SubStream">> = parsexml_xpath:find(Bin, <<"Envelope/Body/GetProfilesResponse/Profiles[1]@token">>),
	undefined = parsexml_xpath:find(Bin, <<"Envelope/Body/GetProfilesResponse/Profiles[2]@token">>),
	Reply = parsexml_xpath:find(Bin, <<"Envelope/Body/GetProfilesResponse">>),
	<<"MainStream">> = parsexml_xpath:find(Reply, <<"Profiles[0]@token">>),
	<<"SubStream">> = parsexml_xpath:find(Reply, <<"Profiles[1]@token">>),
	Profile2 = lists:nth(2, Reply),
	<<"SubStream">> = parsexml_xpath:find(Profile2, <<"Profiles@token">>),
	ok.



find_path_with_nth(_) ->
	ok.





