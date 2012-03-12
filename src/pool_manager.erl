%%%-------------------------------------------------------------------
%%% @author Zsolt Keszthelyi <zsolt@uniss>
%%% @copyright (C) 2012, Zsolt Keszthelyi
%%% @doc
%%%  this is a server for a pool (each pool has a server and a supervisor)
%%%  this is used for adding/removing workers, submitting jobs
%%% @end
%%% Created :  4 Mar 2012 by Zsolt Keszthelyi <zsolt@uniss>
%%%-------------------------------------------------------------------

-module(pool_manager).

-behaviour(gen_server).

-include("stew.hrl").


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(job, {jobid, worklist, donelist}).

-record(state, {
	  poolspec,
	  poolsuppid,
	  workers = [],   %% workers ::[{Pid, JobsNotDone :: integer()}]
	  jobs = []       %% [{From, JobID}]
	 }).    

%%%===================================================================
%%% API
%%%===================================================================
-export([start_link/2, add_workers/2, remove_workers/2, submit_job/2, get_state/1]).

add_workers(PoolServerPID, Num) ->
    gen_server:call(PoolServerPID, {add_workers, Num}).

remove_workers(PoolServerPID, Num) ->
    gen_server:call(PoolServerPID, {remove_workers, Num}).

submit_job(PoolServerPID, JobID, WorkList) 
  when is_list(WorkList)->
    gen_server:call(PoolServerPID, {submit_job, JobID, WorkList}).

submit_job(PoolServerPID, Job) ->
    gen_server:call(PoolServerPID, {submit_job, Job}).

get_state(PoolServerPID)->
    gen_server:call(PoolServerPID, get_state).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(PoolSpec, PoolSupPID) ->
    gen_server:start_link(?MODULE, [PoolSpec, PoolSupPID], []).

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
init([PoolSpec, PoolSupPID]) ->
    register(list_to_atom(PoolSpec#poolspec.poolname++"_manager"),self()),
    
    ExistingWorkers = [ {Pid,0} || {_id, Pid, _type, _modules} <- supervisor2:which_children(PoolSupPID) ],
    %% TODO: this configures existing workers with 0 jobs, that's probably not true -> this will go into negative when existing jobs finish -> keep it at 0 if trying to go negative
    InitialWorkerNum = PoolSpec#poolspec.initial_worker_num,
    ToSpawn = InitialWorkerNum - length(ExistingWorkers),   %% how many new workers do we need
    ToSpawnForReal = 
	if
	    (ToSpawn<0) -> 0;
	    true        -> ToSpawn
	end,
    NewWorkers = add_workers_internal(PoolSpec, ToSpawnForReal),
    %% NewWorkers are not added to the list of workers, they will check in once they are done with init (this way if they crash and a new one gets spawned that will be added to the list)
    {ok, #state{poolspec = PoolSpec, poolsuppid = PoolSupPID, workers = ExistingWorkers}}.

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
handle_call( {submit_job, Job}, _From, #state{poolspec = PoolSpec, workers = Workers, jobs = Jobs} = State) ->
    %% generate jobid
    %% choose worker
    %% send job to worker
    %% put jobid in queue
    JobID = erlang:now(),
    {WorkerPID, WorkerJobNum} = hd(lists:keysort(2, Workers)),  %% the one with the least amount of unfinished work
    WorkerModule = PoolSpec#poolspec.worker_callback,
    WorkerModule:new_job(WorkerPID, JobID, Job),
    NewJobs = [JobID|Jobs],
    NewWorkers = lists:keystore(WorkerPID, 1, Workers, {WorkerPID, WorkerJobNum+1}),  %% one more job for this worker
    ?dv(NewWorkers),
    Reply = ok,
    {reply, Reply, State#state{jobs = NewJobs, workers = NewWorkers}};

handle_call( {submit_job, JobID, WorkList}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call(get_state, _From, State) ->
    Reply = State,
    {reply, Reply, State};

handle_call( {add_workers, Num}, _From, #state{poolspec = PoolSpec} = State) ->
    add_workers_internal(PoolSpec, Num),
    Reply = ok,
    {reply, Reply, State};

handle_call( {remove_workers, Num}, _From, #state{poolspec = PoolSpec} = State) ->
    remove_workers_internal(PoolSpec, State#state.workers, Num),
    Reply = ok,
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
handle_info({job_done, WorkerPID, JobID, Result}, #state{poolspec = PoolSpec, workers = Workers, jobs = Jobs} = State) ->
    NewJobs = Jobs--[JobID],
    {WorkerPID, WorkerJobNum} = lists:keyfind(WorkerPID, 1, Workers),
    NewWorkers = lists:keystore(WorkerPID, 1, Workers, {WorkerPID, WorkerJobNum-1}),  %% one less job for this worker
    ?dv(NewWorkers),
    {noreply, State#state{jobs = NewJobs, workers = NewWorkers}};

handle_info({'DOWN',_Ref,process,Pid,_}, State=#state{workers = Workers}) ->
    %% needs to remove Pid from list of workers if in the list
    NewWorkers = lists:keydelete(Pid, 1, Workers),
    {noreply, State#state{workers = NewWorkers}};


handle_info({Pid, worker_ready}, State=#state{workers = Workers}) ->
    %% needs to add Pid to list of workers
    NewWorkers = [{Pid, 0} | Workers],
    {noreply, State#state{workers = NewWorkers}};

handle_info(Info, State) ->
    ?dv(Info),
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

add_workers_internal(PoolSpec, Num) ->
    PoolSupName = list_to_atom(PoolSpec#poolspec.poolname++"_sup"),
    NewWorkers = lists:map(fun(_) ->
				   {ok, Pid} = supervisor2:start_child(PoolSupName, [self()]),
				   monitor(process, Pid),
				   {Pid, 0}
			   end,
			   lists:seq(1, Num)).

remove_workers_internal(PoolSpec, ExistingWorkers, Num) ->
    lists:foldl(fun
		    (_, []) -> [];
		    (_, [{Pid, _JobNum}|Rest]) -> Pid ! shutdown, Rest
		end,
		ExistingWorkers,
		lists:seq(1, Num)).
