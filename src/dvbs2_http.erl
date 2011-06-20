-module(dvbs2_http).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-record(http, {
  clients,
  socket,
  port
}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Port]) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet,http}, {reuseaddr, true}, {backlog, 30}, {active, false}]),
  {ok, Ref} = prim_inet:async_accept(Listen, -1),
  {ok, #http{
    clients = [],
    socket = Listen,
    port = Port
  }}.

handle_call(_Request, _From, State) ->
  {stop, {unknown_call, _Request}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({inet_async, Listen, _Ref, {ok, CliSocket}}, #http{clients = Clients} = State) ->
  prim_inet:async_accept(Listen, -1),
  case handle_and_subscribe(CliSocket) of
    ok ->
      {noreply, State#http{clients = [CliSocket|Clients]}};
    {error, Error} ->
      ?D({error, Error}),
      {noreply, State}
  end;

handle_info({tcp_closed, Socket}, #http{clients = Clients} = State) ->
  dvbs2:unsubscribe(zvezda, Socket),
  {noreply, State#http{clients = lists:delete(Socket, Clients)}};

handle_info(_Info, State) ->
  ?D({unknown_info, _Info}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_and_subscribe(Socket) ->
  true = inet_db:register_socket(Socket, inet_tcp),
  inet:setopts(Socket, [binary, {packet,http},{active,false},{sndbuf, 65536}]),
  {ok, {http_request, 'GET', {abs_path, Uri}, _Version}} = gen_tcp:recv(Socket, 0),
  case loop_till_eoh(Socket) of
    ok -> reply_and_subscribe(Uri, Socket);
    Else -> Else
  end.  

loop_till_eoh(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, http_eoh} -> inet:setopts(Socket, [{packet,raw},{active,true}]), ok;
    {ok, {http_error, Error}} -> {error, Error};
    {ok, {http_header, _, _, _, _}} -> loop_till_eoh(Socket);
    Else -> {error, Else}
  end.

reply_and_subscribe(Uri, Socket) ->
  gen_tcp:send(Socket, "HTTP/1.0 200 OK\r\nContent-Type: video/mpeg2\r\n\r\n"),
  ?D({subscribe,Socket,zvezda}), 
  dvbs2:subscribe(zvezda, Socket).

  