%%%----------------------------------------------------------------
%%% @author  Benjamin Nortier <bjnortier@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2008 Benjamin Nortier
%%%----------------------------------------------------------------
-module(dayfindr_sup).

-behaviour(supervisor).

%% API
-export([start/0, start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start() ->
    supervisor:start({local, ?SERVER}, ?MODULE, []).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() -> 
    exit(whereis(?MODULE), shutdown).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    RiakClientServer = {riak_client_server,
		   {riak_client_server, start_link, []},
		  Restart, Shutdown, Type, dynamic},

    {ok, {SupFlags, [RiakClientServer]}}.


