#!/usr/bin/env escript

-mode(compile).

main([Path]) ->
  main([Path, "100"]);

main([Path, Count_]) ->
  Count = list_to_integer(Count_),
  {ok,Bin} = file:read_file(Path),
  List = binary_to_list(Bin),
  code:add_pathz("ebin"),
  Self = self(),
  T0 = erlang:now(),
  spawn(fun() -> loop_parsexml(Bin, Count),
    Self ! {ready, parsexml, timer:now_diff(erlang:now(),T0),process_info(self(),memory)} end),
  spawn(fun() -> loop_xmerl(List, Count), 
    Self ! {ready, xmerl, timer:now_diff(erlang:now(),T0),process_info(self(),memory)} end),

  spawn(fun() ->
    code:add_pathz("erlsom/ebin"),
    case code:load_file(erlsom) of
      {error, _} -> Self ! {ready, erlsom, 9999999, {memory,0}};
      _ ->
        loop_erlsom(Bin, Count), 
        Self ! {ready, erlsom, timer:now_diff(erlang:now(),T0),process_info(self(),memory)} 
    end
  end),


  Size = Count*size(Bin),
  receive
    {ready,   xmerl,T1,{memory,M1}} -> io:format("   xmerl: ~8.. Bms ~8.. BKB ~BMB/s~n", [T1 div 1000,M1 div 1024, Size div T1])
  end,
  receive
    {ready,parsexml,T2,{memory,M2}} -> io:format("parsexml: ~8.. Bms ~8.. BKB ~BMB/s~n", [T2 div 1000,M2 div 1024, Size div T2])
  end,
  receive
    {ready,  erlsom,T3,{memory,M3}} -> io:format("  erlsom: ~8.. Bms ~8.. BKB ~BMB/s~n", [T3 div 1000,M3 div 1024, Size div T3])
  end,
  ok.

loop_parsexml(_Bin,0) -> ok;
loop_parsexml(Bin,Count) ->
  _A = parsexml:parse(Bin),
  loop_parsexml(Bin,Count-1).

loop_xmerl(_Bin,0) -> ok;
loop_xmerl(Bin,Count) ->
  _A = xmerl_scan:string(Bin),
  loop_xmerl(Bin,Count-1).

loop_erlsom(_,0) -> ok;
loop_erlsom(Bin,Count) ->
  _A = erlsom:simple_form(Bin),
  loop_erlsom(Bin,Count-1).

