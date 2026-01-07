namespace FSharpAot.Repository

open FSharpAot.Core
open Microsoft.Data.Sqlite
open System.Collections.Generic
open System

type IRepository =
    abstract member Initialize: unit -> unit
    abstract member CreateUser: CreateUserRequest -> User
    abstract member GetUsers: unit -> User seq
    abstract member GetUserById: int -> User option
    abstract member UpdateUser: int * UpdateUserRequest -> User option
    abstract member DeleteUser: int -> bool
    abstract member CreateTask: int * CreateTaskRequest -> Task option
    abstract member GetTasksByUserId: int -> Task seq
    abstract member GetTaskById: int -> Task option
    abstract member UpdateTask: int * UpdateTaskRequest -> Task option
    abstract member MarkTaskDone: int -> Task option
    abstract member DeleteTask: int -> bool

type SqliteRepository(connectionString: string) =
    let executeNonQuery (query: string) (parameters: (string * obj) list) =
        use connection = new SqliteConnection(connectionString)
        connection.Open()
        use command = connection.CreateCommand()
        command.CommandText <- query
        for (name, value) in parameters do
            command.Parameters.AddWithValue(name, value) |> ignore
        command.ExecuteNonQuery()

    let executeScalar (query: string) (parameters: (string * obj) list) =
        use connection = new SqliteConnection(connectionString)
        connection.Open()
        use command = connection.CreateCommand()
        command.CommandText <- query
        for (name, value) in parameters do
            command.Parameters.AddWithValue(name, value) |> ignore
        command.ExecuteScalar()

    let executeReader (query: string) (parameters: (string * obj) list) (mapper: SqliteDataReader -> 'T) =
        use connection = new SqliteConnection(connectionString)
        connection.Open()
        use command = connection.CreateCommand()
        command.CommandText <- query
        for (name, value) in parameters do
            command.Parameters.AddWithValue(name, value) |> ignore
        use reader = command.ExecuteReader()
        let results = ResizeArray<'T>()
        while reader.Read() do
            results.Add(mapper reader)
        results :> IEnumerable<'T>

    interface IRepository with
        member this.Initialize() =
            let tableUsers = """
                CREATE TABLE IF NOT EXISTS users (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    username TEXT UNIQUE NOT NULL,
                    email TEXT UNIQUE NOT NULL
                );
            """
            let tableTasks = """
                CREATE TABLE IF NOT EXISTS tasks (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    user_id INTEGER NOT NULL,
                    title TEXT NOT NULL,
                    description TEXT NOT NULL,
                    done INTEGER NOT NULL DEFAULT 0,
                    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
                );
            """
            executeNonQuery "PRAGMA foreign_keys = ON;" [] |> ignore
            executeNonQuery tableUsers [] |> ignore
            executeNonQuery tableTasks [] |> ignore

        member this.CreateUser(req) =
            let query = "INSERT INTO users (username, email) VALUES (@username, @email) RETURNING id;"
            let id = executeScalar query [("@username", box req.Username); ("@email", box req.Email)]
            { Id = Convert.ToInt32(id); Username = req.Username; Email = req.Email }

        member this.GetUsers() =
            let query = "SELECT id, username, email FROM users;"
            executeReader query [] (fun r ->
                { Id = r.GetInt32(0); Username = r.GetString(1); Email = r.GetString(2) })

        member this.GetUserById(id) =
            let query = "SELECT id, username, email FROM users WHERE id = @id;"
            let users = executeReader query [("@id", box id)] (fun r ->
                { Id = r.GetInt32(0); Username = r.GetString(1); Email = r.GetString(2) })
            users |> Seq.tryHead

        member this.UpdateUser(id, req) =
            let query = "UPDATE users SET username = @username WHERE id = @id RETURNING id, username, email;"
            let users = executeReader query [("@username", box req.Username); ("@id", box id)] (fun r ->
                { Id = r.GetInt32(0); Username = r.GetString(1); Email = r.GetString(2) })
            users |> Seq.tryHead

        member this.DeleteUser(id) =
            let query = "DELETE FROM users WHERE id = @id;"
            let rows = executeNonQuery query [("@id", box id)]
            rows > 0

        member this.CreateTask(userId, req) =
            // Check if user exists
            match (this :> IRepository).GetUserById(userId) with
            | None -> None
            | Some _ ->
                let query = "INSERT INTO tasks (user_id, title, description, done) VALUES (@user_id, @title, @description, 0) RETURNING id;"
                let id = executeScalar query [("@user_id", box userId); ("@title", box req.Title); ("@description", box req.Description)]
                Some { Id = Convert.ToInt32(id); UserId = userId; Title = req.Title; Description = req.Description; Done = false }

        member this.GetTasksByUserId(userId) =
            let query = "SELECT id, user_id, title, description, done FROM tasks WHERE user_id = @user_id;"
            executeReader query [("@user_id", box userId)] (fun r ->
                { Id = r.GetInt32(0); UserId = r.GetInt32(1); Title = r.GetString(2); Description = r.GetString(3); Done = r.GetBoolean(4) })

        member this.GetTaskById(id) =
            let query = "SELECT id, user_id, title, description, done FROM tasks WHERE id = @id;"
            let tasks = executeReader query [("@id", box id)] (fun r ->
                { Id = r.GetInt32(0); UserId = r.GetInt32(1); Title = r.GetString(2); Description = r.GetString(3); Done = r.GetBoolean(4) })
            tasks |> Seq.tryHead

        member this.UpdateTask(id, req) =
            let query = "UPDATE tasks SET title = @title, description = @description WHERE id = @id RETURNING id, user_id, title, description, done;"
            let tasks = executeReader query [("@title", box req.Title); ("@description", box req.Description); ("@id", box id)] (fun r ->
                { Id = r.GetInt32(0); UserId = r.GetInt32(1); Title = r.GetString(2); Description = r.GetString(3); Done = r.GetBoolean(4) })
            tasks |> Seq.tryHead

        member this.MarkTaskDone(id) =
            let query = "UPDATE tasks SET done = 1 WHERE id = @id RETURNING id, user_id, title, description, done;"
            let tasks = executeReader query [("@id", box id)] (fun r ->
                { Id = r.GetInt32(0); UserId = r.GetInt32(1); Title = r.GetString(2); Description = r.GetString(3); Done = r.GetBoolean(4) })
            tasks |> Seq.tryHead

        member this.DeleteTask(id) =
            let query = "DELETE FROM tasks WHERE id = @id;"
            let rows = executeNonQuery query [("@id", box id)]
            rows > 0
