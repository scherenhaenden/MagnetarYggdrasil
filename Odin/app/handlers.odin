package app

import "../server"
import "../sqlite"
import "core:fmt"
import "core:encoding/json"
import "core:strings"
import "core:strconv"
import "core:c"

handle_request :: proc(req: server.Request) -> server.Response {
    path := req.path
    method := req.method

    if path == "/health" {
        return server.Response{200, `{"status": "ok"}`}
    }

    if path == "/users" {
        if method == "POST" {
            return create_user(req.body)
        } else if method == "GET" {
            return list_users()
        }
    }

    if strings.has_prefix(path, "/users/") {
        rest := path[7:] // remove "/users/"

        // Check for /users/{id}/tasks
        if strings.contains(rest, "/tasks") {
             parts := strings.split(rest, "/")
             defer delete(parts)
             if len(parts) == 2 && parts[1] == "tasks" {
                 user_id_str := parts[0]
                 user_id, ok := strconv.parse_int(user_id_str)
                 if ok {
                     if method == "POST" {
                         return create_task(user_id, req.body)
                     } else if method == "GET" {
                         return list_tasks(user_id)
                     }
                 }
             }
        } else {
            // /users/{id}
             user_id, ok := strconv.parse_int(rest)
             if ok {
                 if method == "GET" {
                     return get_user(user_id)
                 } else if method == "PUT" {
                     return update_user(user_id, req.body)
                 } else if method == "DELETE" {
                     return delete_user(user_id)
                 }
             }
        }
    }

    if strings.has_prefix(path, "/tasks/") {
        rest := path[7:]
        // /tasks/{tid}/done
        if strings.has_suffix(rest, "/done") && method == "PATCH" {
             tid_str := rest[:len(rest)-5]
             tid, ok := strconv.parse_int(tid_str)
             if ok {
                 return mark_task_done(tid)
             }
        } else {
             tid, ok := strconv.parse_int(rest)
             if ok {
                 if method == "GET" {
                     return get_task(tid)
                 } else if method == "PUT" {
                     return update_task(tid, req.body)
                 } else if method == "DELETE" {
                     return delete_task(tid)
                 }
             }
        }
    }

    return server.Response{404, `{"error": "not found"}`}
}

// User Handlers

create_user :: proc(body: string) -> server.Response {
    payload: User_Payload
    err := json.unmarshal_string(body, &payload)
    if err != nil {
         return server.Response{400, `{"error": "invalid json"}`}
    }

    stmt: ^sqlite.Statement
    sql := "INSERT INTO users (name, email) VALUES (?, ?)"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)

    c_name := strings.clone_to_cstring(payload.name)
    defer delete(c_name)
    c_email := strings.clone_to_cstring(payload.email)
    defer delete(c_email)

    sqlite.bind_text(stmt, 1, c_name, -1, sqlite.SQLITE_TRANSIENT)
    sqlite.bind_text(stmt, 2, c_email, -1, sqlite.SQLITE_TRANSIENT)

    rc := sqlite.step(stmt)
    sqlite.finalize(stmt)

    if rc != sqlite.DONE {
         return server.Response{500, `{"error": "database error"}`}
    }

    id := sqlite.last_insert_rowid(db_conn)
    return server.Response{201, fmt.tprintf(`{"id": %d, "name": "%s", "email": "%s"}`, id, payload.name, payload.email)}
}

list_users :: proc() -> server.Response {
    stmt: ^sqlite.Statement
    sql := "SELECT id, name, email FROM users"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)

    users: [dynamic]User
    defer delete(users)

    for sqlite.step(stmt) == sqlite.ROW {
        id := int(sqlite.column_int(stmt, 0))
        // Must clone string because column_text memory is transient
        name := strings.clone(string(sqlite.column_text(stmt, 1)))
        email := strings.clone(string(sqlite.column_text(stmt, 2)))
        append(&users, User{id, name, email})
    }
    sqlite.finalize(stmt)

    // Free the cloned strings after marshalling
    defer {
        for u in users {
            delete(u.name)
            delete(u.email)
        }
    }

    json_data, _ := json.marshal(users)
    return server.Response{200, string(json_data)}
}

get_user :: proc(id: int) -> server.Response {
    stmt: ^sqlite.Statement
    sql := "SELECT id, name, email FROM users WHERE id = ?"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
    sqlite.bind_int(stmt, 1, c.int(id))

    if sqlite.step(stmt) == sqlite.ROW {
        u_id := int(sqlite.column_int(stmt, 0))
        name := string(sqlite.column_text(stmt, 1)) // transient view
        email := string(sqlite.column_text(stmt, 2)) // transient view

        // Format response while statement is open and memory is valid
        res_body := fmt.tprintf(`{"id": %d, "name": "%s", "email": "%s"}`, u_id, name, email)

        sqlite.finalize(stmt) // Now we can close
        return server.Response{200, res_body}
    }
    sqlite.finalize(stmt)
    return server.Response{404, `{"error": "user not found"}`}
}

delete_user :: proc(id: int) -> server.Response {
     stmt: ^sqlite.Statement
     sql := "DELETE FROM users WHERE id = ?"
     c_sql := strings.clone_to_cstring(sql)
     defer delete(c_sql)

     sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
     sqlite.bind_int(stmt, 1, c.int(id))
     sqlite.step(stmt)
     sqlite.finalize(stmt)

     if sqlite.changes(db_conn) > 0 {
          return server.Response{200, `{"status": "deleted"}`}
     }
     return server.Response{404, `{"error": "not found"}`}
}

update_user :: proc(id: int, body: string) -> server.Response {
    payload: User_Payload
    err := json.unmarshal_string(body, &payload)
    if err != nil { return server.Response{400, `{"error": "bad json"}`} }

    if payload.name != "" {
         stmt: ^sqlite.Statement
         sql := "UPDATE users SET name = ? WHERE id = ?"
         c_sql := strings.clone_to_cstring(sql)
         defer delete(c_sql)
         sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
         c_name := strings.clone_to_cstring(payload.name)
         defer delete(c_name)
         sqlite.bind_text(stmt, 1, c_name, -1, sqlite.SQLITE_TRANSIENT)
         sqlite.bind_int(stmt, 2, c.int(id))
         sqlite.step(stmt)
         sqlite.finalize(stmt)
    }
    if payload.email != "" {
         stmt: ^sqlite.Statement
         sql := "UPDATE users SET email = ? WHERE id = ?"
         c_sql := strings.clone_to_cstring(sql)
         defer delete(c_sql)
         sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
         c_email := strings.clone_to_cstring(payload.email)
         defer delete(c_email)
         sqlite.bind_text(stmt, 1, c_email, -1, sqlite.SQLITE_TRANSIENT)
         sqlite.bind_int(stmt, 2, c.int(id))
         sqlite.step(stmt)
         sqlite.finalize(stmt)
    }

    return get_user(id)
}

// Task Handlers

create_task :: proc(user_id: int, body: string) -> server.Response {
    payload: Task_Payload
    err := json.unmarshal_string(body, &payload)
    if err != nil { return server.Response{400, `{"error": "bad json"}`} }

    stmt: ^sqlite.Statement
    sql := "INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0)"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
    sqlite.bind_int(stmt, 1, c.int(user_id))

    c_title := strings.clone_to_cstring(payload.title)
    defer delete(c_title)
    c_desc := strings.clone_to_cstring(payload.description)
    defer delete(c_desc)

    sqlite.bind_text(stmt, 2, c_title, -1, sqlite.SQLITE_TRANSIENT)
    sqlite.bind_text(stmt, 3, c_desc, -1, sqlite.SQLITE_TRANSIENT)

    rc := sqlite.step(stmt)
    sqlite.finalize(stmt)

    if rc != sqlite.DONE { return server.Response{500, `{"error": "db error"}`} }

    id := sqlite.last_insert_rowid(db_conn)
    return server.Response{201, fmt.tprintf(`{"id": %d, "user_id": %d, "title": "%s", "description": "%s", "done": false}`, id, user_id, payload.title, payload.description)}
}

list_tasks :: proc(user_id: int) -> server.Response {
    stmt: ^sqlite.Statement
    sql := "SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
    sqlite.bind_int(stmt, 1, c.int(user_id))

    tasks: [dynamic]Task
    defer delete(tasks)

    for sqlite.step(stmt) == sqlite.ROW {
        id := int(sqlite.column_int(stmt, 0))
        u_id := int(sqlite.column_int(stmt, 1))
        // Must clone strings
        title := strings.clone(string(sqlite.column_text(stmt, 2)))
        desc := strings.clone(string(sqlite.column_text(stmt, 3)))
        done := sqlite.column_int(stmt, 4) != 0
        append(&tasks, Task{id, u_id, title, desc, done})
    }
    sqlite.finalize(stmt)

    // Free strings
    defer {
        for t in tasks {
            delete(t.title)
            delete(t.description)
        }
    }

    json_data, _ := json.marshal(tasks)
    return server.Response{200, string(json_data)}
}

get_task :: proc(id: int) -> server.Response {
    stmt: ^sqlite.Statement
    sql := "SELECT id, user_id, title, description, done FROM tasks WHERE id = ?"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
    sqlite.bind_int(stmt, 1, c.int(id))

    if sqlite.step(stmt) == sqlite.ROW {
        t_id := int(sqlite.column_int(stmt, 0))
        u_id := int(sqlite.column_int(stmt, 1))
        title := string(sqlite.column_text(stmt, 2))
        desc := string(sqlite.column_text(stmt, 3))
        done := sqlite.column_int(stmt, 4) != 0

        // Format before closing statement
        res_body := fmt.tprintf(`{"id": %d, "user_id": %d, "title": "%s", "description": "%s", "done": %t}`, t_id, u_id, title, desc, done)

        sqlite.finalize(stmt)
        return server.Response{200, res_body}
    }
    sqlite.finalize(stmt)
    return server.Response{404, `{"error": "task not found"}`}
}

delete_task :: proc(id: int) -> server.Response {
    stmt: ^sqlite.Statement
    sql := "DELETE FROM tasks WHERE id = ?"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
    sqlite.bind_int(stmt, 1, c.int(id))
    sqlite.step(stmt)
    sqlite.finalize(stmt)

    if sqlite.changes(db_conn) > 0 {
        return server.Response{200, `{"status": "deleted"}`}
    }
    return server.Response{404, `{"error": "not found"}`}
}

update_task :: proc(id: int, body: string) -> server.Response {
    payload: Task_Update_Payload
    err := json.unmarshal_string(body, &payload)
    if err != nil { return server.Response{400, `{"error": "bad json"}`} }

    if payload.title != "" {
         stmt: ^sqlite.Statement
         sql := "UPDATE tasks SET title = ? WHERE id = ?"
         c_sql := strings.clone_to_cstring(sql)
         defer delete(c_sql)
         sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
         c_val := strings.clone_to_cstring(payload.title)
         defer delete(c_val)
         sqlite.bind_text(stmt, 1, c_val, -1, sqlite.SQLITE_TRANSIENT)
         sqlite.bind_int(stmt, 2, c.int(id))
         sqlite.step(stmt)
         sqlite.finalize(stmt)
    }
    if payload.description != "" {
         stmt: ^sqlite.Statement
         sql := "UPDATE tasks SET description = ? WHERE id = ?"
         c_sql := strings.clone_to_cstring(sql)
         defer delete(c_sql)
         sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
         c_val := strings.clone_to_cstring(payload.description)
         defer delete(c_val)
         sqlite.bind_text(stmt, 1, c_val, -1, sqlite.SQLITE_TRANSIENT)
         sqlite.bind_int(stmt, 2, c.int(id))
         sqlite.step(stmt)
         sqlite.finalize(stmt)
    }

    return get_task(id)
}

mark_task_done :: proc(id: int) -> server.Response {
    stmt: ^sqlite.Statement
    sql := "UPDATE tasks SET done = 1 WHERE id = ?"
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    sqlite.prepare_v2(db_conn, c_sql, -1, &stmt, nil)
    sqlite.bind_int(stmt, 1, c.int(id))
    sqlite.step(stmt)
    sqlite.finalize(stmt)

    if sqlite.changes(db_conn) > 0 {
         return server.Response{200, `{"status": "marked done"}`}
    }
    return server.Response{404, `{"error": "not found"}`}
}
