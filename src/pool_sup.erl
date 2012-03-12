%%% @author Zsolt Keszthelyi <zsolt@uniss>
%%% @copyright (C) 2012, Zsolt Keszthelyi
%%% @doc
%%%  this is the supervisor over one worker pool
%%% @end
%%% Created :  4 Mar 2012 by Zsolt Keszthelyi <zsolt@uniss>

-module(pool_sup).

-behaviour(supervisor2).

-include("stew.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(PoolSpec) ->
    supervisor2:start_link(?MODULE, [PoolSpec]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% supervisor2 doesn't make sense, if I use 'transient' the child with normal exit will not be respawned (which is the expected behavior), however with {transient, 30} it gets respawned when it shouldn't

init(Args = [PoolSpec]) ->
    register(list_to_atom(PoolSpec#poolspec.poolname++"_sup"),self()),
    ?dv(Args),
    Callback = PoolSpec#poolspec.worker_callback,

    WorkerSpec = {Callback, {Callback, start_link, []}, transient, 5000, worker, [Callback]},
    {ok, { {simple_one_for_one_terminate, 5, 10}, [WorkerSpec]} }.



    %% WorkerSpec = {Callback, {Callback, start_link, []}, {transient, 30}, 5000, worker, [Callback]},
    %% {ok, { {simple_one_for_one_terminate, 5, 10}, [WorkerSpec]} }.
