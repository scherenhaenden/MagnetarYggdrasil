open Models
open Lwt.Infix

module Service = struct
  let handle_db_error err =
    let msg = Caqti_error.show err in
    Lwt.return (Error msg)

  let create_user pool req =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.UserRepo.insert_user (module C) req.username req.email
    ) pool >>= function
    | Ok (Ok user) -> Lwt.return (Ok user)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let list_users pool =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.UserRepo.list_users (module C)
    ) pool >>= function
    | Ok (Ok users) -> Lwt.return (Ok users)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let get_user pool id =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.UserRepo.find_user (module C) id
    ) pool >>= function
    | Ok (Ok user) -> Lwt.return (Ok user)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let update_user pool id req =
    match req.username with
    | None -> get_user pool id (* No update needed *)
    | Some username ->
        Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
          Repository.UserRepo.update_user (module C) id username
        ) pool >>= function
        | Ok (Ok user) -> Lwt.return (Ok user)
        | Ok (Error e) -> handle_db_error e
        | Error e -> handle_db_error e

  let delete_user pool id =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.UserRepo.delete_user (module C) id
    ) pool >>= function
    | Ok (Ok ()) -> Lwt.return (Ok ())
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let create_task pool user_id req =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
       Repository.UserRepo.find_user (module C) user_id >>= function
       | Error e -> Lwt.return (Error e)
       | Ok None -> Lwt.return (Ok None)
       | Ok (Some _) ->
           Repository.TaskRepo.insert_task (module C) user_id req.title req.description >>= function
           | Error e -> Lwt.return (Error e)
           | Ok task -> Lwt.return (Ok (Some task))
    ) pool >>= function
    | Ok (Ok (Some task)) -> Lwt.return (Ok (Some task))
    | Ok (Ok None) -> Lwt.return (Ok None) (* User not found *)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let list_tasks pool user_id =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
       Repository.UserRepo.find_user (module C) user_id >>= function
       | Error e -> Lwt.return (Error e)
       | Ok None -> Lwt.return (Ok None)
       | Ok (Some _) ->
           Repository.TaskRepo.list_tasks (module C) user_id >>= function
           | Error e -> Lwt.return (Error e)
           | Ok tasks -> Lwt.return (Ok (Some tasks))
    ) pool >>= function
    | Ok (Ok (Some tasks)) -> Lwt.return (Ok (Some tasks))
    | Ok (Ok None) -> Lwt.return (Ok None) (* User not found *)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let get_task pool id =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.TaskRepo.find_task (module C) id
    ) pool >>= function
    | Ok (Ok task) -> Lwt.return (Ok task)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let update_task pool id req =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.TaskRepo.find_task (module C) id >>= function
      | Error e -> Lwt.return (Error e)
      | Ok None -> Lwt.return (Ok None)
      | Ok (Some current_task) ->
          let title = Option.value req.title ~default:current_task.title in
          let description = Option.value req.description ~default:current_task.description in
          Repository.TaskRepo.update_task (module C) id title description
    ) pool >>= function
    | Ok (Ok task) -> Lwt.return (Ok task)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let patch_task_done pool id =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.TaskRepo.mark_done (module C) id
    ) pool >>= function
    | Ok (Ok task) -> Lwt.return (Ok task)
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

  let delete_task pool id =
     Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
      Repository.TaskRepo.delete_task (module C) id
    ) pool >>= function
    | Ok (Ok ()) -> Lwt.return (Ok ())
    | Ok (Error e) -> handle_db_error e
    | Error e -> handle_db_error e

end
