-module(magnetar_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
        #{id => magnetar_db,
          start => {magnetar_db, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [magnetar_db]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
