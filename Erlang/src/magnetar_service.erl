-module(magnetar_service).

-export([create_user/2, get_users/0, get_user/1, update_user/3, delete_user/1]).
-export([create_task/3, get_tasks/1, get_task/1, update_task/3, mark_task_done/1, delete_task/1]).

%% The service layer mostly delegates to DB for this simple app,
%% but it acts as a facade.

create_user(Name, Email) ->
    magnetar_db:create_user(Name, Email).

get_users() ->
    magnetar_db:get_users().

get_user(Id) ->
    magnetar_db:get_user(Id).

update_user(Id, Name, Email) ->
    magnetar_db:update_user(Id, Name, Email).

delete_user(Id) ->
    magnetar_db:delete_user(Id).

create_task(UserId, Title, Description) ->
    magnetar_db:create_task(UserId, Title, Description).

get_tasks(UserId) ->
    magnetar_db:get_tasks(UserId).

get_task(TaskId) ->
    magnetar_db:get_task(TaskId).

update_task(TaskId, Title, Description) ->
    magnetar_db:update_task(TaskId, Title, Description, undefined).

mark_task_done(TaskId) ->
    magnetar_db:mark_task_done(TaskId).

delete_task(TaskId) ->
    magnetar_db:delete_task(TaskId).
