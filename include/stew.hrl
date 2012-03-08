%%% @author Zsolt Keszthelyi <zsolt@uniss>
%%% @copyright (C) 2012, Zsolt Keszthelyi
%%% @doc
%%%   header file (record, macro defs)
%%% @end
%%% Created :  4 Mar 2012 by Zsolt Keszthelyi <zsolt@uniss>

-define(TRANSCHILD(I, Type, Args), {I, {I, start_link, Args}, transient, 5000, Type, [I]}).
-define(TEMPCHILD(I, Type, Args),  {I, {I, start_link, Args}, temporary, 5000, Type, [I]}).
-define(PERMCHILD(I, Type, Args),  {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(PoolSupSpec(PoolName, PoolSpec), {{PoolName, sup}, {pool_sup, start_link, [PoolSpec]}, {permanent, 30}, 5000, supervisor, [pool_sup]}).
-define(PoolManagerSpec(PoolName, PoolSpec, PoolSupPID), {{PoolName, manager}, {pool_manager, start_link, [PoolSpec, PoolSupPID]}, {permanent, 30}, 5000, worker, [pool_server]}).

-define(PGV(Key, List), proplists:get_value(Key, List)).

-define( d(Msg, Args), io:format("~15s::"++Msg++"~n", [?MODULE|Args]) ).
-define( d(Msg), io:format("~15s::~p~n", [?MODULE, Msg]) ).
-define( dv(Var), io:format("~15s::~s = ~p~n", [?MODULE, ??Var, Var]) ).



-record(poolspec, {
		   poolname  :: string(),
		   initial_worker_num        :: integer(),
		   max_worker_num            :: integer(),
		   worker_callback           :: atom()
	 }).
