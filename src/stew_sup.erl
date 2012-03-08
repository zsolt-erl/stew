%%% @author Zsolt Keszthelyi <zsolt@uniss>
%%% @copyright (C) 2012, Zsolt Keszthelyi
%%% @doc
%%%  this is the app supervisor, main_manager and main_sup runs under this, there's a pool_manager and a pool_sup for every pool under main_sup
%%% @end
%%% Created :  4 Mar 2012 by Zsolt Keszthelyi <zsolt@uniss>

-module(stew_sup).

-behaviour(supervisor).

-include("stew.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = ?PERMCHILD(main_manager, worker, []),
    Sup    = ?PERMCHILD(main_sup, supervisor, []),

    {ok, { {one_for_one, 5, 10}, [Server, Sup]} }.

