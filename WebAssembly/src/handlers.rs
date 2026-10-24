use spin_sdk::http::{Request, Response, IntoResponse};
use anyhow::Result;
use serde::Serialize;
use serde_json::json;
use crate::service::Service;
use crate::traits::RepositoryInterface;
use crate::models::{CreateUser, UpdateUser, CreateTask, UpdateTask};
use regex::Regex;

pub struct Handlers<R: RepositoryInterface> {
    service: Service<R>,
}

impl<R: RepositoryInterface> Handlers<R> {
    pub fn new(service: Service<R>) -> Self {
        Self { service }
    }

    fn json_response<T: Serialize>(data: T, status: u16) -> Response {
        let body = serde_json::to_vec(&data).unwrap();
        Response::builder()
            .status(status)
            .header("content-type", "application/json")
            .body(body)
            .build()
    }

    fn error_response(msg: &str, status: u16) -> Response {
        let body = serde_json::to_vec(&json!({"error": msg})).unwrap();
        Response::builder()
            .status(status)
            .header("content-type", "application/json")
            .body(body)
            .build()
    }

    pub fn handle_request(&self, req: Request) -> Result<Response> {
        // In Spin SDK 3+, Request is http::Request. req.uri() returns &http::Uri.
        // However, previous compilation error said "no method named `path` found for reference `&str`"
        // which implies req.uri() returned a &str in that context, OR I was confused.
        // But usually http::Uri HAS a .path() method.
        // If req.uri() returned &str, then .path() would fail.
        // Let's look at the error again:
        // `error[E0599]: no method named path found for reference &str in the current scope`
        // --> src/handlers.rs:39:24
        // 39 |         let path = uri.path();
        // This means `uri` variable WAS a `&str`.
        // So `req.uri()` returned `&str`.
        // If `req.uri()` returns `&str`, then `path` IS the path (or full URI string).
        // So I should just use `uri` as `path` if it's already a string.
        // But if it is a full URI, I might need to parse it.
        // However, if the compiler says it's &str, I can't call .path() on it.
        // So `let path = uri;` is correct if `uri` is the path string.

        let uri = req.uri();
        let path = uri;

        let method = req.method();

        // Router logic (manual since we don't have a router framework in Spin yet without extra deps)

        // System
        if path == "/health" && method == &spin_sdk::http::Method::Get {
            return Ok(Self::json_response(json!({"status": "ok", "version": "1.0.0"}), 200));
        }

        // Users
        // GET /users
        if path == "/users" && method == &spin_sdk::http::Method::Get {
             return match self.service.get_users() {
                Ok(users) => Ok(Self::json_response(users, 200)),
                Err(e) => Ok(Self::error_response(&e.to_string(), 500)),
             };
        }

        // POST /users
        if path == "/users" && method == &spin_sdk::http::Method::Post {
            let body = req.body();
            let payload: CreateUser = match serde_json::from_slice(body) {
                Ok(p) => p,
                Err(_) => return Ok(Self::error_response("Invalid JSON", 400)),
            };

            return match self.service.create_user(payload) {
                Ok(user) => Ok(Self::json_response(user, 201)),
                Err(e) => Ok(Self::error_response(&e.to_string(), 400)), // Assuming logic errors are 400
            };
        }

        // GET /users/{id}
        let re_user_id = Regex::new(r"^/users/(\d+)$").unwrap();
        if let Some(caps) = re_user_id.captures(path) {
            let id = caps[1].parse::<i64>().unwrap();

            if method == &spin_sdk::http::Method::Get {
                 return match self.service.get_user(id) {
                    Ok(user) => Ok(Self::json_response(user, 200)),
                    Err(_) => Ok(Self::error_response("User not found", 404)),
                 };
            }

            // PUT /users/{id}
            if method == &spin_sdk::http::Method::Put {
                let body = req.body();
                let payload: UpdateUser = match serde_json::from_slice(body) {
                    Ok(p) => p,
                    Err(_) => return Ok(Self::error_response("Invalid JSON", 400)),
                };
                return match self.service.update_user(id, payload) {
                     Ok(user) => Ok(Self::json_response(user, 200)),
                     Err(_) => Ok(Self::error_response("User not found", 404)),
                };
            }

            // DELETE /users/{id}
            if method == &spin_sdk::http::Method::Delete {
                 return match self.service.delete_user(id) {
                     Ok(_) => Ok(Response::builder().status(204).body(()).build()),
                     Err(_) => Ok(Self::error_response("User not found", 404)),
                 };
            }
        }

        // Tasks
        // POST /users/{id}/tasks
        let re_user_tasks = Regex::new(r"^/users/(\d+)/tasks$").unwrap();
        if let Some(caps) = re_user_tasks.captures(path) {
            let user_id = caps[1].parse::<i64>().unwrap();

            if method == &spin_sdk::http::Method::Post {
                 let body = req.body();
                 let payload: CreateTask = match serde_json::from_slice(body) {
                     Ok(p) => p,
                     Err(_) => return Ok(Self::error_response("Invalid JSON", 400)),
                 };
                 return match self.service.create_task(user_id, payload) {
                     Ok(task) => Ok(Self::json_response(task, 201)),
                     Err(_) => Ok(Self::error_response("User not found", 404)),
                 };
            }

            // GET /users/{id}/tasks
             if method == &spin_sdk::http::Method::Get {
                 return match self.service.get_tasks(user_id) {
                     Ok(tasks) => Ok(Self::json_response(tasks, 200)),
                     Err(_) => Ok(Self::error_response("User not found", 404)),
                 };
            }
        }

        // Task by ID operations
        let re_task_id = Regex::new(r"^/tasks/(\d+)$").unwrap();
        if let Some(caps) = re_task_id.captures(path) {
             let id = caps[1].parse::<i64>().unwrap();

             // GET /tasks/{id}
             if method == &spin_sdk::http::Method::Get {
                  return match self.service.get_task(id) {
                      Ok(task) => Ok(Self::json_response(task, 200)),
                      Err(_) => Ok(Self::error_response("Task not found", 404)),
                  };
             }

             // PUT /tasks/{id}
             if method == &spin_sdk::http::Method::Put {
                  let body = req.body();
                  let payload: UpdateTask = match serde_json::from_slice(body) {
                      Ok(p) => p,
                      Err(_) => return Ok(Self::error_response("Invalid JSON", 400)),
                  };
                  return match self.service.update_task(id, payload) {
                      Ok(task) => Ok(Self::json_response(task, 200)),
                      Err(_) => Ok(Self::error_response("Task not found", 404)),
                  };
             }

             // DELETE /tasks/{id}
              if method == &spin_sdk::http::Method::Delete {
                  return match self.service.delete_task(id) {
                      Ok(_) => Ok(Response::builder().status(204).body(()).build()),
                      Err(_) => Ok(Self::error_response("Task not found", 404)),
                  };
             }
        }

        // PATCH /tasks/{id}/done
        let re_task_done = Regex::new(r"^/tasks/(\d+)/done$").unwrap();
        if let Some(caps) = re_task_done.captures(path) {
             let id = caps[1].parse::<i64>().unwrap();
             if method == &spin_sdk::http::Method::Patch {
                   return match self.service.mark_task_done(id) {
                       Ok(task) => Ok(Self::json_response(task, 200)),
                       Err(_) => Ok(Self::error_response("Task not found", 404)),
                   };
             }
        }

        Ok(Self::error_response("Not Found", 404))
    }
}
