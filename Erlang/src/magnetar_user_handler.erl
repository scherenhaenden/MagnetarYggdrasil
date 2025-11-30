-module(magnetar_user_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    BindingId = cowboy_req:binding(id, Req0),
    handle(Method, BindingId, Req0, State).

%% GET /users
handle(<<"GET">>, undefined, Req, State) ->
    {ok, Users} = magnetar_service:get_users(),
    Body = jsx:encode(Users),
    reply_json(200, Body, Req, State);

%% POST /users
handle(<<"POST">>, undefined, Req, State) ->
    {ok, Data, Req2} = read_body(Req),
    #{<<"name">> := Name, <<"email">> := Email} = Data,
    {ok, NewId} = magnetar_service:create_user(Name, Email),
    %% Return created user
    User = #{id => NewId, name => Name, email => Email},
    reply_json(201, jsx:encode(User), Req2, State);

%% GET /users/:id
handle(<<"GET">>, IdBin, Req, State) ->
    Id = binary_to_integer(IdBin),
    case magnetar_service:get_user(Id) of
        {ok, User} -> reply_json(200, jsx:encode(User), Req, State);
        {error, not_found} -> reply_json(404, <<"{\"error\":\"User not found\"}">>, Req, State)
    end;

%% PUT /users/:id
handle(<<"PUT">>, IdBin, Req, State) ->
    Id = binary_to_integer(IdBin),
    {ok, Data, Req2} = read_body(Req),
    %% Name/Email optional? "name?, email?"
    Name = maps:get(<<"name">>, Data, undefined),
    Email = maps:get(<<"email">>, Data, undefined),
    case magnetar_service:update_user(Id, Name, Email) of
        ok ->
            {ok, User} = magnetar_service:get_user(Id),
            reply_json(200, jsx:encode(User), Req2, State);
        {error, not_found} -> reply_json(404, <<"{\"error\":\"User not found\"}">>, Req2, State)
    end;

%% DELETE /users/:id
handle(<<"DELETE">>, IdBin, Req, State) ->
    Id = binary_to_integer(IdBin),
    magnetar_service:delete_user(Id),
    reply_json(204, <<>>, Req, State);

handle(_, _, Req, State) ->
    reply_json(405, <<"{\"error\":\"Method not allowed\"}">>, Req, State).


read_body(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    {ok, Data, Req2}.

reply_json(Status, Body, Req, State) ->
    Req2 = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req),
    {ok, Req2, State}.
