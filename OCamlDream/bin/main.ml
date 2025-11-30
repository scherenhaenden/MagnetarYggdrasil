open Magnetar_lib
open Lwt.Infix

let () =
  let db_url = "sqlite3:magnetar.db" in
  let pool = Database.Db.pool db_url in

  let setup_db =
    Database.Db.init pool >>= function
    | Ok () -> Lwt.return_unit
    | Error e ->
        Printf.eprintf "Database initialization failed: %s\n" (Caqti_error.show e);
        exit 1
  in

  Lwt_main.run (
    setup_db >>= fun () ->
    Printf.printf "Starting server at http://localhost:8080\n";
    Dream.serve ~interface:"0.0.0.0" ~port:8080
      (Dream.logger @@ Handlers.router pool)
  )
