#!/usr/bin/env ERL_LIBS=.. escript
%%! -pa ebin

-define(TIME, 3).

main([]) ->
  application:start(dvbs2),
  {ok, DVB} = dvbs2_sup:start_dvbs2(zvezda, [
    {lo_freq, 10600}, {hi_freq, 10750}, {freq, 12640}, {srate, 30000}, {adapter, 0}, {tuner, 0}, {pids, [0, 104, 1104, 2104]}
  ]),
  dvbs2_sup:start_http_listener(8085),
  dvbs2:subscribe(DVB, self()),
  put(counter, 0),
  put(total, 0),
  timer:send_interval(?TIME*1000, flush),
  {ok, F} = file:open("output.ts", [write, binary]),
  io:format("Opened output ~p~n", [F]),
  put(writer, F),
  loop(),
  receive
    Msg -> io:format("Msg: ~p~n", [Msg])
  end,
  ok.


loop() ->
  Counter = get(counter),
  receive
    {dvb, _Port, Bin} ->
      put(total, get(total) + size(Bin)),
      % io:format("~p ~p~n", [Counter, size(Bin)]),
      put(counter, get(counter) + 1),
      file:write(get(writer), Bin),
      loop();
    flush ->
      io:format("~p Kb/s~n", [round(get(total)*8 / (?TIME*1024))]),
      put(counter, 0),
      put(total, 0),
      loop();
    Else ->
      io:format("~p~n", [Else]),
      loop()
    after
      1000 ->
        ok  
  end.