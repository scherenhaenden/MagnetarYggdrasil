-module(magnetar_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Ensure DB is initialized
    magnetar_db:init_db(),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", magnetar_health_handler, []},
            {"/users", magnetar_user_handler, []},
            {"/users/:id", magnetar_user_handler, []},
            {"/users/:id/tasks", magnetar_task_handler, []},
            {"/tasks/:tid", magnetar_task_handler, []},
            {"/tasks/:tid/done", magnetar_task_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    magnetar_sup:start_link().

stop(_State) ->
    ok.
