%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Special TCP driver
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(dvbs2).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-behaviour(gen_server).


-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([subscribe/2]).
-export([unsubscribe/2]).

-export([open/1, get_pid/2, start_flow/1]).

-record(dvbs2, {
  dvb,
  clients
}).

start_link(Name, Config) ->
  gen_server:start_link({local, Name}, ?MODULE, [Config], []).

start_link(Config) ->
  gen_server:start_link(?MODULE, [Config], []).

subscribe(DVB, Socket) ->
  gen_server:call(DVB, {subscribe, self(), Socket}).

unsubscribe(DVB, Socket) ->
  gen_server:call(DVB, {unsubscribe, self(), Socket}).

init([Config]) ->
  {ok, DVB} = open(Config),
  dvbs2:get_pid(DVB, proplists:get_value(pids, Config)),
  dvbs2:start_flow(DVB),
  {ok, #dvbs2{dvb = DVB, clients = []}}.

handle_call({subscribe, Pid, Port}, _From, #dvbs2{clients = Clients} = State) ->
  case lists:keyfind(Port, 2, Clients) of
    {_, _} ->
      {reply, {error, already_subscribe}, State};
    false ->
      Ref = erlang:monitor(process, Pid),
      {reply, ok, State#dvbs2{clients = [{Pid, Port}|Clients]}}
  end;

handle_call({unsubscribe, _Pid, Port}, _From, #dvbs2{clients = Clients} = State) ->
  Clients1 = lists:keydelete(Port, 2, Clients),
  {reply, ok, State#dvbs2{clients = Clients1}};

handle_call(_Request, _From, State) ->
  {stop, {unknown_call, _Request}, State}.

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info({inet_reply,_Port,_Reply}, State) ->
  {noreply, State};

handle_info({dvb_ready, DVB}, #dvbs2{dvb = DVB} = State) ->
  {noreply, State};

handle_info({dvb, DVB, Bin}, #dvbs2{dvb = DVB, clients = Clients} = State) ->
  lists:foreach(fun
    ({_Pid, Port}) when is_port(Port) -> (catch port_command(Port, Bin, [nosuspend]));
    ({_Pid, Pid}) when is_pid(Pid) -> Pid ! {dvb, self(), Bin}
  end, Clients),
  {noreply, State};
  
handle_info(_Info, State) ->
  ?D({unknown_msg, _Info}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


-define(CMD_OPEN, 1).
-define(CMD_GET_PID, 2).
-define(CMD_START_INPUT, 3).


open(Options) ->
  case erl_ddll:load_driver(code:lib_dir(dvbs2,ebin), dvbs2_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> exit({error, {could_not_load_driver,erl_ddll:format_error(Error)}})
  end,
  DVB = open_port({spawn, dvbs2_drv}, [binary]),
  
  LoFrequency = proplists:get_value(lo_freq, Options, 10600)*1000,
  HiFrequency = proplists:get_value(hi_freq, Options, 10750)*1000,
  Frequency = round(proplists:get_value(freq, Options, 12640)*1000),
  SRate = proplists:get_value(srate, Options, 30000)*1000,
  Adapter = proplists:get_value(adapter, Options, 0),
  Tuner = proplists:get_value(tuner, Options, 0),
  Polarization = case proplists:get_value(polarization, Options, v) of
     v  -> $V;
    'V' -> $V;
     r  -> $V;
    'R' -> $V;
     h  -> $H;
    'H' -> $H;
     l  -> $H;
    'L' -> $H
  end,
  
  <<"ok">> = port_control(DVB, ?CMD_OPEN, <<LoFrequency:32/little, HiFrequency:32/little, Frequency:32/little, SRate:32/little, Adapter, Tuner, Polarization>>),
  io:format("Opening frontend ~p~n", [DVB]),
  receive
    {dvb_ready, DVB} -> ok
  end,
  io:format("Opened frontend ~p~n", [DVB]),
  {ok, DVB}.

get_pid(DVB, Pid) when is_number(Pid) ->
  <<"ok">> = port_control(DVB, ?CMD_GET_PID, <<Pid:16/little>>);

get_pid(DVB, Pids) when is_list(Pids) ->
  [get_pid(DVB, Pid) || Pid <- Pids].


start_flow(DVB) ->
  <<"ok">> = port_control(DVB, ?CMD_START_INPUT, <<>>).

