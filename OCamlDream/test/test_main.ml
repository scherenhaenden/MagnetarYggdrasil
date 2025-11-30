open Magnetar_lib
open Lwt.Infix

(* Mock Database/Service Logic for Testing *)
(* Since we can't easily mock the Caqti pool without a complex functor setup or an interface,
   we will focus on testing the JSON conversion and logic if possible,
   or set up an in-memory SQLite database for integration tests. *)

(* Integration Test Setup using in-memory SQLite *)

let db_uri = "sqlite3::memory:"

let pool = Database.Db.pool db_uri

let setup () =
  Database.Db.init pool >>= function
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.fail (Caqti_error.show e)

(* Helpers *)
let create_user username email =
  Service.Service.create_user pool { username; email }

let test_user_flow _ () =
  setup () >>= fun () ->
  create_user "testuser" "test@example.com" >>= function
  | Ok user ->
      Alcotest.(check int) "check id" 1 user.id;
      Alcotest.(check string) "check username" "testuser" user.username;

      Service.Service.get_user pool user.id >>= fun res ->
      (match res with
       | Ok (Some u) -> Alcotest.(check string) "check fetched username" "testuser" u.username
       | _ -> Alcotest.fail "Failed to fetch user");

      Lwt.return_unit
  | Error e -> Alcotest.fail e

let test_task_flow _ () =
  setup () >>= fun () ->
  create_user "tasker" "tasker@example.com" >>= fun res ->
  match res with
  | Ok user ->
      Service.Service.create_task pool user.id { title = "T1"; description = "D1" } >>= fun t_res ->
      (match t_res with
      | Ok (Some task) ->
          Alcotest.(check string) "check title" "T1" task.title;
          Service.Service.patch_task_done pool task.id >>= fun u_res ->
          (match u_res with
          | Ok (Some updated) -> Alcotest.(check bool) "check done" true updated.done_
          | _ -> Alcotest.fail "Failed to update task")
      | _ -> Alcotest.fail "Failed to create task");
      Lwt.return_unit
  | Error e -> Alcotest.fail e

let () =
  Lwt_main.run @@ Alcotest_lwt.run "Magnetar Tests" [
    "integration", [
      "user_flow", `Quick, test_user_flow;
      "task_flow", `Quick, test_task_flow;
    ]
  ]
