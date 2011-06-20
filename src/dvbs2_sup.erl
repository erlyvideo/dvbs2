
-module(dvbs2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_dvbs2/2, start_http_listener/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_dvbs2(Name, Config) ->
  DVBS2 = {
    {Name, sup},
    {dvbs2, start_link ,[Name, Config]},
    permanent,
    10000,
    worker,
    [dvbs2]
  },
  supervisor:start_child(?MODULE, DVBS2).


start_http_listener(Port) ->
  HTTP = {
    dvbs2_http_sup,
    {dvbs2_http, start_link ,[Port]},
    permanent,
    10000,
    worker,
    [dvbs2_http]
  },
  supervisor:start_child(?MODULE, HTTP).
  

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

