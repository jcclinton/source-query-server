-module(server_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{query_server,
					{query_server, start_link, []},
					transient, 10000, worker, [query_server]}],
	{ok, {{one_for_one, 60, 3600}, Procs}}.
