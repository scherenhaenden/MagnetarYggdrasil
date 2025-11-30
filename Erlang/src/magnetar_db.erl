-module(magnetar_db).
-behaviour(gen_server).

%% API
-export([start_link/0, init_db/0]).
-export([create_user/2, get_users/0, get_user/1, update_user/3, delete_user/1]).
-export([create_task/3, get_tasks/1, get_task/1, update_task/4, mark_task_done/1, delete_task/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DB_PATH, "magnetar.db").

-record(state, {conn}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init_db() ->
    %% We open a temporary connection just to ensure tables exist
    %% ideally this is done by the gen_server on init, but app start calls this.
    %% Let's just rely on gen_server init doing the work if possible.
    %% But if app calls init_db() before sup starts, we have a race.
    %% Better: let the gen_server do it on init.
    ok.

create_user(Name, Email) ->
    gen_server:call(?SERVER, {create_user, Name, Email}).

get_users() ->
    gen_server:call(?SERVER, get_users).

get_user(Id) ->
    gen_server:call(?SERVER, {get_user, Id}).

update_user(Id, Name, Email) ->
    gen_server:call(?SERVER, {update_user, Id, Name, Email}).

delete_user(Id) ->
    gen_server:call(?SERVER, {delete_user, Id}).

create_task(UserId, Title, Description) ->
    gen_server:call(?SERVER, {create_task, UserId, Title, Description}).

get_tasks(UserId) ->
    gen_server:call(?SERVER, {get_tasks, UserId}).

get_task(TaskId) ->
    gen_server:call(?SERVER, {get_task, TaskId}).

update_task(TaskId, Title, Description, IsDone) ->
    gen_server:call(?SERVER, {update_task, TaskId, Title, Description, IsDone}).

mark_task_done(TaskId) ->
    gen_server:call(?SERVER, {mark_task_done, TaskId}).

delete_task(TaskId) ->
    gen_server:call(?SERVER, {delete_task, TaskId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Conn} = esqlite3:open(?DB_PATH),
    ok = ensure_schema(Conn),
    {ok, #state{conn = Conn}}.

handle_call({create_user, Name, Email}, _From, State) ->
    Q = "INSERT INTO users (name, email) VALUES (?, ?);",
    Result = case esqlite3:exec(Q, [Name, Email], State#state.conn) of
        ok ->
            LastId = esqlite3:last_insert_rowid(State#state.conn),
            {ok, LastId};
        {error, Reason} ->
             {error, Reason}
    end,
    {reply, Result, State};

handle_call(get_users, _From, State) ->
    Q = "SELECT id, name, email FROM users;",
    Result = esqlite3:q(Q, [], State#state.conn),
    Rows = [ #{id => Id, name => Name, email => Email} || {Id, Name, Email} <- Result ],
    {reply, {ok, Rows}, State};

handle_call({get_user, Id}, _From, State) ->
    Q = "SELECT id, name, email FROM users WHERE id = ?;",
    Result = esqlite3:q(Q, [Id], State#state.conn),
    Reply = case Result of
        [{RId, Name, Email}] -> {ok, #{id => RId, name => Name, email => Email}};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({update_user, Id, Name, Email}, _From, State) ->
    %% Check if exists first or just update.
    %% User might provide Name or Email as null/undefined.
    %% But here we assume the handler handles logic of what to update,
    %% or we implement dynamic update.
    %% For simplicity let's assume we fetch, merge, then update or we do dynamic query.
    %% Let's assume the caller passes current values if they are not to be changed, or we handle it here.
    %% The API spec says PUT /users/{id} (name?, email?).

    %% To make it robust:
    QSelect = "SELECT name, email FROM users WHERE id = ?;",
    case esqlite3:q(QSelect, [Id], State#state.conn) of
        [{CurrName, CurrEmail}] ->
            NewName = case Name of undefined -> CurrName; _ -> Name end,
            NewEmail = case Email of undefined -> CurrEmail; _ -> Email end,
            QUpdate = "UPDATE users SET name = ?, email = ? WHERE id = ?;",
            esqlite3:exec(QUpdate, [NewName, NewEmail, Id], State#state.conn),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_user, Id}, _From, State) ->
    Q = "DELETE FROM users WHERE id = ?;",
    esqlite3:exec(Q, [Id], State#state.conn),
    {reply, ok, State};

handle_call({create_task, UserId, Title, Description}, _From, State) ->
    %% Check if user exists
    QCheck = "SELECT 1 FROM users WHERE id = ?;",
    case esqlite3:q(QCheck, [UserId], State#state.conn) of
        [] -> {reply, {error, not_found}, State};
        _ ->
            Q = "INSERT INTO tasks (user_id, title, description, is_done) VALUES (?, ?, ?, 0);",
            Result = case esqlite3:exec(Q, [UserId, Title, Description], State#state.conn) of
                ok ->
                    LastId = esqlite3:last_insert_rowid(State#state.conn),
                    {ok, LastId};
                {error, Reason} -> {error, Reason}
            end,
            {reply, Result, State}
    end;

handle_call({get_tasks, UserId}, _From, State) ->
    Q = "SELECT id, user_id, title, description, is_done FROM tasks WHERE user_id = ?;",
    Result = esqlite3:q(Q, [UserId], State#state.conn),
    Rows = [ #{id => Id, user_id => UId, title => Title, description => Desc, is_done => bool(IsDone)}
             || {Id, UId, Title, Desc, IsDone} <- Result ],
    {reply, {ok, Rows}, State};

handle_call({get_task, TaskId}, _From, State) ->
    Q = "SELECT id, user_id, title, description, is_done FROM tasks WHERE id = ?;",
    Result = esqlite3:q(Q, [TaskId], State#state.conn),
    Reply = case Result of
        [{Id, UId, Title, Desc, IsDone}] ->
            {ok, #{id => Id, user_id => UId, title => Title, description => Desc, is_done => bool(IsDone)}};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({update_task, TaskId, Title, Description, IsDone}, _From, State) ->
    QSelect = "SELECT title, description, is_done FROM tasks WHERE id = ?;",
    case esqlite3:q(QSelect, [TaskId], State#state.conn) of
        [{CurrTitle, CurrDesc, CurrIsDone}] ->
            NewTitle = case Title of undefined -> CurrTitle; _ -> Title end,
            NewDesc = case Description of undefined -> CurrDesc; _ -> Description end,
            NewIsDone = case IsDone of undefined -> CurrIsDone; _ -> int(IsDone) end,

            QUpdate = "UPDATE tasks SET title = ?, description = ?, is_done = ? WHERE id = ?;",
            esqlite3:exec(QUpdate, [NewTitle, NewDesc, NewIsDone, TaskId], State#state.conn),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({mark_task_done, TaskId}, _From, State) ->
     QUpdate = "UPDATE tasks SET is_done = 1 WHERE id = ?;",
     %% We should check if it exists first to return 404 correctly if needed,
     %% or we can check changes().
     esqlite3:exec(QUpdate, [TaskId], State#state.conn),
     Changes = esqlite3:changes(State#state.conn),
     Reply = if Changes > 0 -> ok; true -> {error, not_found} end,
     {reply, Reply, State};

handle_call({delete_task, TaskId}, _From, State) ->
    Q = "DELETE FROM tasks WHERE id = ?;",
    esqlite3:exec(Q, [TaskId], State#state.conn),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    esqlite3:close(State#state.conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure_schema(Conn) ->
    QUsers = "CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        email TEXT NOT NULL
    );",
    ok = esqlite3:exec(QUsers, Conn),

    QTasks = "CREATE TABLE IF NOT EXISTS tasks (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL,
        title TEXT NOT NULL,
        description TEXT NOT NULL,
        is_done INTEGER DEFAULT 0,
        FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
    );",
    ok = esqlite3:exec(QTasks, Conn),

    %% Enable foreign keys
    ok = esqlite3:exec("PRAGMA foreign_keys = ON;", Conn),
    ok.

bool(0) -> false;
bool(1) -> true;
bool(true) -> true;
bool(false) -> false.

int(true) -> 1;
int(false) -> 0;
int(1) -> 1;
int(0) -> 0.
