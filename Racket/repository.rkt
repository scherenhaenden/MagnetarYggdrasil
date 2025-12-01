#lang racket

(require db
         "db.rkt"
         "models.rkt")

(provide create-user
         get-all-users
         get-user-by-id
         update-user
         delete-user
         create-task
         get-tasks-by-user-id
         get-task-by-id
         update-task
         mark-task-done
         delete-task)

(define (create-user username email)
  (define conn (get-db-conn))
  (query-exec conn "INSERT INTO users (username, email) VALUES (?, ?)" username email)
  (define id (query-value conn "SELECT last_insert_rowid()"))
  (disconnect conn)
  (user id username email))

(define (get-all-users)
  (define conn (get-db-conn))
  (define rows (query-rows conn "SELECT id, username, email FROM users"))
  (disconnect conn)
  (map (lambda (row) (apply user (vector->list row))) rows))

(define (get-user-by-id id)
  (define conn (get-db-conn))
  (define row (query-maybe-row conn "SELECT id, username, email FROM users WHERE id = ?" id))
  (disconnect conn)
  (if row (apply user (vector->list row)) #f))

(define (update-user id username email)
  (define conn (get-db-conn))
  (query-exec conn "UPDATE users SET username = ?, email = ? WHERE id = ?" username email id)
  (disconnect conn)
  (get-user-by-id id))

(define (delete-user id)
  (define conn (get-db-conn))
  (query-exec conn "DELETE FROM users WHERE id = ?" id)
  (disconnect conn))

(define (create-task user-id title description)
  (define conn (get-db-conn))
  (query-exec conn "INSERT INTO tasks (user_id, title, description, status) VALUES (?, ?, ?, 'todo')" user-id title description)
  (define id (query-value conn "SELECT last_insert_rowid()"))
  (disconnect conn)
  (task id user-id title description "todo"))

(define (get-tasks-by-user-id user-id)
  (define conn (get-db-conn))
  (define rows (query-rows conn "SELECT id, user_id, title, description, status FROM tasks WHERE user_id = ?" user-id))
  (disconnect conn)
  (map (lambda (row) (apply task (vector->list row))) rows))

(define (get-task-by-id id)
  (define conn (get-db-conn))
  (define row (query-maybe-row conn "SELECT id, user_id, title, description, status FROM tasks WHERE id = ?" id))
  (disconnect conn)
  (if row (apply task (vector->list row)) #f))

(define (update-task id title description)
  (define conn (get-db-conn))
  (query-exec conn "UPDATE tasks SET title = ?, description = ? WHERE id = ?" title description id)
  (disconnect conn)
  (get-task-by-id id))

(define (mark-task-done id)
  (define conn (get-db-conn))
  (query-exec conn "UPDATE tasks SET status = 'done' WHERE id = ?" id)
  (disconnect conn)
  (get-task-by-id id))

(define (delete-task id)
  (define conn (get-db-conn))
  (query-exec conn "DELETE FROM tasks WHERE id = ?" id)
  (disconnect conn))
