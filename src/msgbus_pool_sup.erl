%%% -------------------------------------------------------------------
%%% Author  :   BlackAnimal  <ronalfei@gmail.com> or <ronalfei@qq.com> 
%%% Description :
%%%
%%% Created : 2011-7-11
%%% -------------------------------------------------------------------
-module(msgbus_pool_sup).
-author("BlackAnimal <ronalfei@gmail.com> or <ronalfei@qq.com>").

-behaviour(supervisor).

-include("snail.hrl").
%% External exports
-export([start_link/0, stop/0, restart/0, reload/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
	supervisor:which_children(?MODULE).

restart() ->
	supervisor:which_children(?MODULE).
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	Workers = get_msgbus_workers(),
	lager:debug("Worker configs : ~p, supervisor pid : ~p", [Workers, self()]),
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, Workers}
	}.


%------------------------------------------------

get_msgbus_workers() ->
	{ok, ConfigList} = file:consult("src/msgbus_pool.conf"),	
	%[  parseConfig(tuple_to_list(X))   || X <- lists:flatten(ConfigList)].
	[  parseConfig(X)   || X <- ConfigList ].


parseConfig(PropConfig) ->
	Worker = proplists:get_value("queue", PropConfig),
	lager:debug("~p, configlist: ~p", [Worker, PropConfig]),
	{
		Worker,
		{msgbus_pool, start_link, [PropConfig]},
		permanent,
		5000,
		worker,
		dynamic
	}.

reload() ->
	Workers = get_msgbus_workers(),
	[ supervisor:start_child(?MODULE,X) || X <- Workers].
%%------------------------------------------------
%get_child_spec() ->
%	{ok,
%		{
%			{one_for_one,10,10},
%			{cpool_server,
%				{cpool_server, start, []},
%				permanent, 5000, worker, dynamic}
%		}
%	}.
%-------------------------------------------------
