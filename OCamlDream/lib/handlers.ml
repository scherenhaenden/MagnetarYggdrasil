open Dream
open Models
open Lwt.Infix

let to_json conv data =
  Yojson.Safe.to_string (conv data)

let json_response conv data status =
  json ~status (to_json conv data)

let error_response status message =
  json ~status (to_json yojson_of_error_response { error = message })

let parse_json conv req =
  body req >>= fun body_str ->
  try
    let json = Yojson.Safe.from_string body_str in
    Lwt.return (Ok (conv json))
  with _ -> Lwt.return (Error "Invalid JSON")

let health _ =
  json_response yojson_of_health_response { status = "ok"; version = "1.0.0" } `OK

let create_user pool req =
  parse_json create_user_request_of_yojson req >>= function
  | Error msg -> error_response `Bad_Request msg
  | Ok user_req ->
      Service.Service.create_user pool user_req >>= function
      | Ok user -> json_response yojson_of_user user `Created
      | Error msg -> error_response `Bad_Request msg (* Likely unique constraint *)

let list_users pool _ =
  Service.Service.list_users pool >>= function
  | Ok users -> json_response (fun l -> `List (List.map yojson_of_user l)) users `OK
  | Error msg -> error_response `Internal_Server_Error msg

let get_user pool id _ =
  Service.Service.get_user pool (int_of_string id) >>= function
  | Ok (Some user) -> json_response yojson_of_user user `OK
  | Ok None -> error_response `Not_Found "User not found"
  | Error msg -> error_response `Internal_Server_Error msg

let update_user pool id req =
  parse_json update_user_request_of_yojson req >>= function
  | Error msg -> error_response `Bad_Request msg
  | Ok user_req ->
      Service.Service.update_user pool (int_of_string id) user_req >>= function
      | Ok (Some user) -> json_response yojson_of_user user `OK
      | Ok None -> error_response `Not_Found "User not found"
      | Error msg -> error_response `Internal_Server_Error msg

let delete_user pool id _ =
  Service.Service.delete_user pool (int_of_string id) >>= function
  | Ok () -> empty `No_Content
  | Error msg -> error_response `Internal_Server_Error msg

let create_task pool id req =
  parse_json create_task_request_of_yojson req >>= function
  | Error msg -> error_response `Bad_Request msg
  | Ok task_req ->
      Service.Service.create_task pool (int_of_string id) task_req >>= function
      | Ok (Some task) -> json_response yojson_of_task task `Created
      | Ok None -> error_response `Not_Found "User not found"
      | Error msg -> error_response `Internal_Server_Error msg

let list_tasks pool id _ =
  Service.Service.list_tasks pool (int_of_string id) >>= function
  | Ok (Some tasks) -> json_response (fun l -> `List (List.map yojson_of_task l)) tasks `OK
  | Ok None -> error_response `Not_Found "User not found"
  | Error msg -> error_response `Internal_Server_Error msg

let get_task pool id _ =
  Service.Service.get_task pool (int_of_string id) >>= function
  | Ok (Some task) -> json_response yojson_of_task task `OK
  | Ok None -> error_response `Not_Found "Task not found"
  | Error msg -> error_response `Internal_Server_Error msg

let update_task pool id req =
  parse_json update_task_request_of_yojson req >>= function
  | Error msg -> error_response `Bad_Request msg
  | Ok task_req ->
      Service.Service.update_task pool (int_of_string id) task_req >>= function
      | Ok (Some task) -> json_response yojson_of_task task `OK
      | Ok None -> error_response `Not_Found "Task not found"
      | Error msg -> error_response `Internal_Server_Error msg

let patch_task_done pool id _ =
  Service.Service.patch_task_done pool (int_of_string id) >>= function
  | Ok (Some task) -> json_response yojson_of_task task `OK
  | Ok None -> error_response `Not_Found "Task not found"
  | Error msg -> error_response `Internal_Server_Error msg

let delete_task pool id _ =
  Service.Service.delete_task pool (int_of_string id) >>= function
  | Ok () -> empty `No_Content
  | Error msg -> error_response `Internal_Server_Error msg

let router pool =
  Dream.router [
    get "/health" health;

    post "/users" (create_user pool);
    get "/users" (list_users pool);
    get "/users/:id" (fun req -> get_user pool (param req "id") req);
    put "/users/:id" (fun req -> update_user pool (param req "id") req);
    delete "/users/:id" (fun req -> delete_user pool (param req "id") req);

    post "/users/:id/tasks" (fun req -> create_task pool (param req "id") req);
    get "/users/:id/tasks" (fun req -> list_tasks pool (param req "id") req);

    get "/tasks/:id" (fun req -> get_task pool (param req "id") req);
    put "/tasks/:id" (fun req -> update_task pool (param req "id") req);
    patch "/tasks/:id/done" (fun req -> patch_task_done pool (param req "id") req);
    delete "/tasks/:id" (fun req -> delete_task pool (param req "id") req);
  ]
