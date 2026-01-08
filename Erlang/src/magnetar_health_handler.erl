-module(magnetar_health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        <<"{\"status\":\"ok\",\"version\":\"1.0.0\"}">>,
        Req0),
    {ok, Req, State}.
