import std/db_sqlite
import std/os

var DbPath* = "magnetar.db"

proc getDb*(): DbConn =
  result = open(DbPath, "", "", "")

proc initDatabase*() =
  let db = getDb()
  defer: db.close()

  db.exec(sql"""
    PRAGMA foreign_keys = ON;
  """)

  db.exec(sql"""
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      email TEXT NOT NULL UNIQUE,
      name TEXT NOT NULL
    );
  """)

  db.exec(sql"""
    CREATE TABLE IF NOT EXISTS tasks (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      title TEXT NOT NULL,
      description TEXT NOT NULL,
      done BOOLEAN NOT NULL DEFAULT 0,
      FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
    );
  """)

  # Indices for performance (as per requirements)
  db.exec(sql"""
    CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
  """)
  db.exec(sql"""
    CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id);
  """)
