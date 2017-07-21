%%%-------------------------------------------------------------------
%% @doc up_config top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(up_config_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Children = [
    xfutils:child_spec(gws_up_config)
  ],
  RestartStrategy = xfutils:sup_restart_strategy(),
  {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================










