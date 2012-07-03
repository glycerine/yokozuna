-module(yz_sup).
-behaviour(supervisor).
-include("yokozuna.hrl").
-export([start_link/0]).
-export([init/1]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_Args) ->
    VMaster = {?YZ_VNODE_MASTER,
               {riak_core_vnode_master, start_link, [yz_vnode]},
               permanent, 5000, worker, [riak_core_vnode_master]},

    {ok, {{one_for_one, 5, 10}, [VMaster]}}.
