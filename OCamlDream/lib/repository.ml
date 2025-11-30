open Models
open Lwt.Infix
open Caqti_request.Infix

module UserRepo = struct
  let create =
    (Caqti_type.t2 Caqti_type.string Caqti_type.string ->. Caqti_type.unit)
    "INSERT INTO users (username, email) VALUES (?, ?)"

  let get_last_insert_id =
    (Caqti_type.unit ->. Caqti_type.int)
    "SELECT last_insert_rowid()"

  let get_all =
    (Caqti_type.unit ->* Caqti_type.t3 Caqti_type.int Caqti_type.string Caqti_type.string)
    "SELECT id, username, email FROM users"

  let get_by_id =
    (Caqti_type.int ->! Caqti_type.t3 Caqti_type.int Caqti_type.string Caqti_type.string)
    "SELECT id, username, email FROM users WHERE id = ?"

  let update_username =
    (Caqti_type.t2 Caqti_type.string Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET username = ? WHERE id = ?"

  let delete =
    (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM users WHERE id = ?"

  let insert_user (module C : Caqti_lwt.CONNECTION) username email =
    C.exec create (username, email) >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () ->
        C.find get_last_insert_id () >>= function
        | Error e -> Lwt.return (Error e)
        | Ok id -> Lwt.return (Ok { id; username; email })

  let list_users (module C : Caqti_lwt.CONNECTION) =
    C.collect_list get_all () >>= function
    | Error e -> Lwt.return (Error e)
    | Ok rows ->
        let users = List.map (fun (id, username, email) -> { id; username; email }) rows in
        Lwt.return (Ok users)

  let find_user (module C : Caqti_lwt.CONNECTION) id =
    C.find_opt get_by_id id >>= function
    | Error e -> Lwt.return (Error e)
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some (id, username, email)) -> Lwt.return (Ok (Some { id; username; email }))

  let update_user (module C : Caqti_lwt.CONNECTION) id username =
    C.exec update_username (username, id) >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () -> find_user (module C) id

  let delete_user (module C : Caqti_lwt.CONNECTION) id =
    C.exec delete id
end

module TaskRepo = struct
  let create =
    (Caqti_type.t3 Caqti_type.int Caqti_type.string Caqti_type.string ->. Caqti_type.unit)
    "INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0)"

  let get_last_insert_id =
    (Caqti_type.unit ->. Caqti_type.int)
    "SELECT last_insert_rowid()"

  let get_by_user =
    (Caqti_type.int ->* Caqti_type.t5 Caqti_type.int Caqti_type.int Caqti_type.string Caqti_type.string Caqti_type.bool)
    "SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?"

  let get_by_id =
    (Caqti_type.int ->! Caqti_type.t5 Caqti_type.int Caqti_type.int Caqti_type.string Caqti_type.string Caqti_type.bool)
    "SELECT id, user_id, title, description, done FROM tasks WHERE id = ?"

  let update =
    (Caqti_type.t3 Caqti_type.string Caqti_type.string Caqti_type.int ->. Caqti_type.unit)
    "UPDATE tasks SET title = ?, description = ? WHERE id = ?"

  let update_done =
    (Caqti_type.t2 Caqti_type.bool Caqti_type.int ->. Caqti_type.unit)
    "UPDATE tasks SET done = ? WHERE id = ?"

  let delete =
    (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM tasks WHERE id = ?"

  let insert_task (module C : Caqti_lwt.CONNECTION) user_id title description =
    C.exec create (user_id, title, description) >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () ->
        C.find get_last_insert_id () >>= function
        | Error e -> Lwt.return (Error e)
        | Ok id -> Lwt.return (Ok { id; user_id; title; description; done_ = false })

  let list_tasks (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_by_user user_id >>= function
    | Error e -> Lwt.return (Error e)
    | Ok rows ->
        let tasks = List.map (fun (id, user_id, title, description, done_) -> { id; user_id; title; description; done_ }) rows in
        Lwt.return (Ok tasks)

  let find_task (module C : Caqti_lwt.CONNECTION) id =
    C.find_opt get_by_id id >>= function
    | Error e -> Lwt.return (Error e)
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some (id, user_id, title, description, done_)) -> Lwt.return (Ok (Some { id; user_id; title; description; done_ }))

  let update_task (module C : Caqti_lwt.CONNECTION) id title description =
    C.exec update (title, description, id) >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () -> find_task (module C) id

  let mark_done (module C : Caqti_lwt.CONNECTION) id =
     C.exec update_done (true, id) >>= function
     | Error e -> Lwt.return (Error e)
     | Ok () -> find_task (module C) id

  let delete_task (module C : Caqti_lwt.CONNECTION) id =
    C.exec delete id
end
