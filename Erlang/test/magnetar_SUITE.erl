-module(magnetar_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [health_test, user_crud_test, task_crud_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(magnetar),
    Config.

end_per_suite(_Config) ->
    application:stop(magnetar),
    application:stop(inets),
    ok.

health_test(_Config) ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {"http://localhost:8080/health", []}, [], []),
    ?assertMatch(<<"{\"status\":\"ok\"}">>, list_to_binary(Body)).

user_crud_test(_Config) ->
    %% Create
    UserJson = "{\"name\":\"Alice\",\"email\":\"alice@example.com\"}",
    {ok, {{_, 201, _}, _, Body1}} = httpc:request(post, {"http://localhost:8080/users", [], "application/json", UserJson}, [], []),
    #{<<"id">> := Id, <<"name">> := <<"Alice">>} = jsx:decode(list_to_binary(Body1), [return_maps]),

    %% Get List
    {ok, {{_, 200, _}, _, Body2}} = httpc:request(get, {"http://localhost:8080/users", []}, [], []),
    List = jsx:decode(list_to_binary(Body2), [return_maps]),
    ?assert(length(List) > 0),

    %% Get One
    UrlId = "http://localhost:8080/users/" ++ integer_to_list(Id),
    {ok, {{_, 200, _}, _, Body3}} = httpc:request(get, {UrlId, []}, [], []),
    #{<<"name">> := <<"Alice">>} = jsx:decode(list_to_binary(Body3), [return_maps]),

    %% Update
    UpdateJson = "{\"name\":\"Alice Updated\"}",
    {ok, {{_, 200, _}, _, Body4}} = httpc:request(put, {UrlId, [], "application/json", UpdateJson}, [], []),
    #{<<"name">> := <<"Alice Updated">>} = jsx:decode(list_to_binary(Body4), [return_maps]),

    %% Delete
    {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {UrlId, []}, [], []),

    %% Verify Delete
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {UrlId, []}, [], []),
    ok.

task_crud_test(_Config) ->
    %% Create User
    UserJson = "{\"name\":\"Bob\",\"email\":\"bob@example.com\"}",
    {ok, {{_, 201, _}, _, BodyU}} = httpc:request(post, {"http://localhost:8080/users", [], "application/json", UserJson}, [], []),
    #{<<"id">> := UserId} = jsx:decode(list_to_binary(BodyU), [return_maps]),

    %% Create Task
    TaskJson = "{\"title\":\"Work\",\"description\":\"Do work\"}",
    UrlTasks = "http://localhost:8080/users/" ++ integer_to_list(UserId) ++ "/tasks",
    {ok, {{_, 201, _}, _, BodyT}} = httpc:request(post, {UrlTasks, [], "application/json", TaskJson}, [], []),
    #{<<"id">> := TaskId, <<"title">> := <<"Work">>} = jsx:decode(list_to_binary(BodyT), [return_maps]),

    %% Get Tasks
    {ok, {{_, 200, _}, _, BodyTList}} = httpc:request(get, {UrlTasks, []}, [], []),
    TList = jsx:decode(list_to_binary(BodyTList), [return_maps]),
    ?assert(length(TList) > 0),

    %% Get Task
    UrlTask = "http://localhost:8080/tasks/" ++ integer_to_list(TaskId),
    {ok, {{_, 200, _}, _, BodyT2}} = httpc:request(get, {UrlTask, []}, [], []),
    #{<<"title">> := <<"Work">>} = jsx:decode(list_to_binary(BodyT2), [return_maps]),

    %% Mark Done
    UrlDone = UrlTask ++ "/done",
    {ok, {{_, 200, _}, _, _}} = httpc:request(patch, {UrlDone, [], "application/json", ""}, [], []),

    %% Verify Done
    {ok, {{_, 200, _}, _, BodyT3}} = httpc:request(get, {UrlTask, []}, [], []),
    #{<<"is_done">> := true} = jsx:decode(list_to_binary(BodyT3), [return_maps]),

    %% Delete Task
    {ok, {{_, 204, _}, _, _}} = httpc:request(delete, {UrlTask, []}, [], []),

    %% Verify Delete
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {UrlTask, []}, [], []),
    ok.
