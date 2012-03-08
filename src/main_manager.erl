%%%-------------------------------------------------------------------
%%% @author Zsolt Keszthelyi <zsolt@uniss>
%%% @copyright (C) 2012, Zsolt Keszthelyi
%%% @doc
%%%  server to create/delete/keep track of pools
%%% @end
%%% Created :  4 Mar 2012 by Zsolt Keszthelyi <zsolt@uniss>
%%%-------------------------------------------------------------------
-module(main_manager).

-behaviour(gen_server).

-include("stew.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {pools = []}).     %% [{PoolName, PoolSpec, ServerPID, SupPID}]

%%%===================================================================
%%% API
%%%===================================================================
-export([add_pool/1, remove_pool/1, get_pool_list/0, get_pool_manager/1]).

add_pool(PoolSpec) 
  when is_record(PoolSpec, poolspec)->
    gen_server:call(?MODULE, {add_pool, PoolSpec}).

remove_pool(PoolName) ->
    gen_server:call(?MODULE, {remove_pool, PoolName}).

get_pool_list() ->
    gen_server:call(?MODULE, get_pool_list).

get_pool_manager(PoolName) ->
    gen_server:call(?MODULE, {get_pool_manager, PoolName}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({add_pool, #poolspec{poolname = PoolName} = PoolSpec}, _From, #state{pools = Pools} = State) ->
    %% TODO: poolname is part of poolspec, should not be specified separately in call -> rewrite macro!
    {ok, PoolSupPID} =    supervisor2:start_child(main_sup, ?PoolSupSpec(PoolName, PoolSpec)),
    ?dv(PoolSupPID),
    {ok, PoolServerPID} = supervisor2:start_child(main_sup, ?PoolManagerSpec(PoolName, PoolSpec, PoolSupPID)),
    ?dv(PoolServerPID),
    NewPools = [{PoolName, PoolSpec, PoolServerPID, PoolSupPID}|Pools],
    Reply = PoolServerPID,
    {reply, Reply, State#state{pools = NewPools}};

handle_call(get_pool_list, _From, #state{pools = Pools} = State) ->
    Reply = Pools,
    {reply, Reply, State};

handle_call({get_pool_manager, PoolName}, _From, #state{pools = Pools} = State) ->
    Reply = 
	case lists:keyfind(PoolName, 1, Pools) of
	    false -> 
		{error, nonex_pool};
	    {PoolName, _PoolSpec, ServerPID, _SupPID} ->
		ServerPID
	end,	 
    {reply, Reply, State};


handle_call({remove_pool, PoolName}, _From, #state{pools = Pools} = State) ->
    Reply = 
	case lists:keyfind(PoolName, 1, Pools) of
	    false -> 
		{error, nonex_pool};
	    {PoolName, _PoolSpec, ServerPID, _SupPID} ->
		pool_server:terminate(ServerPID)
	end,	 
    {reply, Reply, State};



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

