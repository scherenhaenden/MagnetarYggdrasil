#lang racket

(require db)

(provide init-db
         get-db-conn)

(define db-path "magnetar.sqlite")

(define (get-db-conn)
  (sqlite3-connect #:database db-path #:mode 'create))

(define (init-db)
  (define conn (get-db-conn))
  (query-exec conn "PRAGMA foreign_keys = ON")
  (query-exec conn "CREATE TABLE IF NOT EXISTS users (
                      id INTEGER PRIMARY KEY AUTOINCREMENT,
                      username TEXT NOT NULL,
                      email TEXT NOT NULL UNIQUE
                    )")
  (query-exec conn "CREATE TABLE IF NOT EXISTS tasks (
                      id INTEGER PRIMARY KEY AUTOINCREMENT,
                      user_id INTEGER NOT NULL,
                      title TEXT NOT NULL,
                      description TEXT,
                      status TEXT DEFAULT 'todo',
                      FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
                    )")
  (disconnect conn))
