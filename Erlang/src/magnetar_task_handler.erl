-module(magnetar_task_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    BindingTid = cowboy_req:binding(tid, Req0),
    BindingUid = cowboy_req:binding(id, Req0),
    Path = cowboy_req:path(Req0),

    %% Determine if it is /tasks/:tid/done
    IsDonePath = case BindingTid of
        undefined -> false;
        _ ->
            %% Check if suffix is /done
            Size = byte_size(Path),
            case Size > 5 of
                true ->
                    Suffix = binary_part(Path, Size, -5),
                    Suffix =:= <<"/done">>;
                false -> false
            end
    end,

    handle(Method, BindingUid, BindingTid, IsDonePath, Req0, State).

%% GET /users/:id/tasks
handle(<<"GET">>, UserIdBin, undefined, false, Req, State) when UserIdBin =/= undefined ->
    UserId = binary_to_integer(UserIdBin),
    {ok, Tasks} = magnetar_service:get_tasks(UserId),
    reply_json(200, jsx:encode(Tasks), Req, State);

%% POST /users/:id/tasks
handle(<<"POST">>, UserIdBin, undefined, false, Req, State) when UserIdBin =/= undefined ->
    UserId = binary_to_integer(UserIdBin),
    {ok, Data, Req2} = read_body(Req),
    #{<<"title">> := Title, <<"description">> := Desc} = Data,
    case magnetar_service:create_task(UserId, Title, Desc) of
        {ok, NewId} ->
            Task = #{id => NewId, user_id => UserId, title => Title, description => Desc, is_done => false},
            reply_json(201, jsx:encode(Task), Req2, State);
        {error, not_found} ->
            reply_json(404, <<"{\"error\":\"User not found\"}">>, Req2, State);
        {error, _} ->
            reply_json(500, <<"{\"error\":\"Internal error\"}">>, Req2, State)
    end;

%% GET /tasks/:tid
handle(<<"GET">>, undefined, TaskIdBin, false, Req, State) when TaskIdBin =/= undefined ->
    TaskId = binary_to_integer(TaskIdBin),
    case magnetar_service:get_task(TaskId) of
        {ok, Task} -> reply_json(200, jsx:encode(Task), Req, State);
        {error, not_found} -> reply_json(404, <<"{\"error\":\"Task not found\"}">>, Req, State)
    end;

%% PUT /tasks/:tid
handle(<<"PUT">>, undefined, TaskIdBin, false, Req, State) when TaskIdBin =/= undefined ->
    TaskId = binary_to_integer(TaskIdBin),
    {ok, Data, Req2} = read_body(Req),
    Title = maps:get(<<"title">>, Data, undefined),
    Desc = maps:get(<<"description">>, Data, undefined),
    case magnetar_service:update_task(TaskId, Title, Desc) of
        ok ->
            {ok, Task} = magnetar_service:get_task(TaskId),
            reply_json(200, jsx:encode(Task), Req2, State);
        {error, not_found} -> reply_json(404, <<"{\"error\":\"Task not found\"}">>, Req2, State)
    end;

%% PATCH /tasks/:tid/done
handle(<<"PATCH">>, undefined, TaskIdBin, true, Req, State) when TaskIdBin =/= undefined ->
    TaskId = binary_to_integer(TaskIdBin),
    case magnetar_service:mark_task_done(TaskId) of
        ok -> reply_json(200, <<"{\"status\":\"ok\"}">>, Req, State);
        {error, not_found} -> reply_json(404, <<"{\"error\":\"Task not found\"}">>, Req, State)
    end;

%% DELETE /tasks/:tid
handle(<<"DELETE">>, undefined, TaskIdBin, false, Req, State) when TaskIdBin =/= undefined ->
    TaskId = binary_to_integer(TaskIdBin),
    magnetar_service:delete_task(TaskId),
    reply_json(204, <<>>, Req, State);

handle(_, _, _, _, Req, State) ->
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
