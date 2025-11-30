package app

import "../sqlite"
import "core:c"
import "core:fmt"
import "core:strings"

db_conn: ^sqlite.Connection

init_db :: proc(filename: string = "magnetar.db") {
    c_filename := strings.clone_to_cstring(filename)
    defer delete(c_filename)

    rc := sqlite.open(c_filename, &db_conn)
    if rc != sqlite.OK {
        fmt.println("Can't open database:", sqlite.errmsg(db_conn))
        return
    }

    exec_sql("PRAGMA foreign_keys = ON;")

    // Create Users table
    exec_sql(`CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        email TEXT NOT NULL UNIQUE
    );`)

    // Create Tasks table
    exec_sql(`CREATE TABLE IF NOT EXISTS tasks (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL,
        title TEXT NOT NULL,
        description TEXT NOT NULL,
        done BOOLEAN NOT NULL DEFAULT 0,
        FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
    );`)

    // Create Indices
    exec_sql("CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);")
    exec_sql("CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id);")
}

exec_sql :: proc(sql: string) {
    err: cstring
    c_sql := strings.clone_to_cstring(sql)
    defer delete(c_sql)

    rc := sqlite.exec(db_conn, c_sql, nil, nil, &err)
    if rc != sqlite.OK {
        fmt.printf("SQL Error: %s\n", err)
        // sqlite.free(err)
    }
}
