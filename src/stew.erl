%%% @author Zsolt Keszthelyi <zsolt@uniss>
%%% @copyright (C) 2012, Zsolt Keszthelyi
%%% @doc
%%%  wrapper to start stew app.
%%% @end
%%% Created :  4 Mar 2012 by Zsolt Keszthelyi <zsolt@uniss>

-module(stew).

-include("stew.hrl").

-export([start/0, stop/0, test/0]).
-compile(export_all).

start()->
    application:start(stew).

stop()->
    application:stop(stew).



test()->
    Server = main_manager:add_pool({poolspec, "pool1", 5, 10, worker}),
    ?d("added pool1"),
    ?dv(Server),
    pool_manager:submit_job(Server, 6),
    ?d("submitted job"),
    ok.


test2()->
    Server = main_manager:get_pool_manager("pool1"),
    [pool_manager:submit_job(Server, J) || J <- [1,2,3,4,5]].

test3()->
    Server = main_manager:add_pool({poolspec, "pool2", 2, 10, worker}),
    ?d("added pool2"),
    ?dv(Server),
    pool_manager:submit_job(Server, 8),
    ?d("submitted job"),
    ok.
