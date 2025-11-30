open Lwt.Infix
open Caqti_request.Infix

(* Define the connection pool type *)
type connection = (Caqti_lwt.connection, Caqti_error.t) result Lwt.t

module Db = struct
  let connect uri =
    Caqti_lwt.connect (Uri.of_string uri)

  let pool uri =
    match Caqti_lwt.connect_pool (Uri.of_string uri) with
    | Ok pool -> pool
    | Error err -> failwith (Caqti_error.show err)

  let migrate_users =
    (Caqti_type.unit ->. Caqti_type.unit)
    {| CREATE TABLE IF NOT EXISTS users (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         username TEXT NOT NULL UNIQUE,
         email TEXT NOT NULL UNIQUE
       ) |}

  let migrate_tasks =
    (Caqti_type.unit ->. Caqti_type.unit)
    {| CREATE TABLE IF NOT EXISTS tasks (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         user_id INTEGER NOT NULL,
         title TEXT NOT NULL,
         description TEXT NOT NULL,
         done BOOLEAN NOT NULL DEFAULT 0,
         FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE
       ) |}

  let enable_foreign_keys =
    (Caqti_type.unit ->. Caqti_type.unit)
    "PRAGMA foreign_keys = ON"

  let init pool =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      C.exec enable_foreign_keys () >>= fun r1 ->
      match r1 with
      | Error e -> Lwt.return (Error e)
      | Ok () ->
        C.exec migrate_users () >>= fun r2 ->
        match r2 with
        | Error e -> Lwt.return (Error e)
        | Ok () ->
          C.exec migrate_tasks ()
    ) pool
end
