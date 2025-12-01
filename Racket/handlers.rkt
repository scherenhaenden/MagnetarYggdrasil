#lang racket

(require web-server/servlet
         web-server/servlet-env
         json
         "service.rkt"
         "models.rkt")

(provide start-server)

(define (response/json data #:code [code 200])
  (response/full
   code
   (if (= code 200) #"OK" #"Error")
   (current-seconds)
   #"application/json; charset=utf-8"
   (list (header #"Content-Type" #"application/json; charset=utf-8"))
   (list (jsexpr->bytes data))))

(define (parse-json-body req)
  (bytes->jsexpr (request-post-data/raw req)))

(define (user->json u)
  (hasheq 'id (user-id u)
          'username (user-username u)
          'email (user-email u)))

(define (task->json t)
  (hasheq 'id (task-id t)
          'user_id (task-user_id t)
          'title (task-title t)
          'description (task-description t)
          'status (task-status t)))

;; Handlers

(define (health-check req)
  (response/json (hasheq 'status "ok")))

(define (create-user-handler req)
  (define data (parse-json-body req))
  (define u (create-user-service (hash-ref data 'username) (hash-ref data 'email)))
  (response/json (user->json u) #:code 201))

(define (list-users-handler req)
  (define users (list-users-service))
  (response/json (map user->json users)))

(define (get-user-handler req id)
  (define u (get-user-service (string->number id)))
  (if u
      (response/json (user->json u))
      (response/json (hasheq 'error "User not found") #:code 404)))

(define (update-user-handler req id)
  (define data (parse-json-body req))
  (define u (update-user-service (string->number id) (hash-ref data 'username) (hash-ref data 'email)))
  (if u
      (response/json (user->json u))
      (response/json (hasheq 'error "User not found") #:code 404)))

(define (delete-user-handler req id)
  (delete-user-service (string->number id))
  (response/json (hasheq 'status "deleted")))

(define (create-task-handler req user-id)
  (define data (parse-json-body req))
  (define t (create-task-service (string->number user-id) (hash-ref data 'title) (hash-ref data 'description)))
  (response/json (task->json t) #:code 201))

(define (list-tasks-handler req user-id)
  (define tasks (list-user-tasks-service (string->number user-id)))
  (response/json (map task->json tasks)))

(define (get-task-handler req id)
  (define t (get-task-service (string->number id)))
  (if t
      (response/json (task->json t))
      (response/json (hasheq 'error "Task not found") #:code 404)))

(define (update-task-handler req id)
  (define data (parse-json-body req))
  (define t (update-task-service (string->number id) (hash-ref data 'title) (hash-ref data 'description)))
  (if t
      (response/json (task->json t))
      (response/json (hasheq 'error "Task not found") #:code 404)))

(define (complete-task-handler req id)
  (define t (complete-task-service (string->number id)))
  (if t
      (response/json (task->json t))
      (response/json (hasheq 'error "Task not found") #:code 404)))

(define (delete-task-handler req id)
  (delete-task-service (string->number id))
  (response/json (hasheq 'status "deleted")))

;; Router

(define (dispatch req)
  (define path (map path/param-path (url-path (request-uri req))))
  (define method (request-method req))

  (match (list method path)
    [(list #"GET" '("health")) (health-check req)]

    [(list #"POST" '("users")) (create-user-handler req)]
    [(list #"GET" '("users")) (list-users-handler req)]
    [(list #"GET" (list "users" id)) (get-user-handler req id)]
    [(list #"PUT" (list "users" id)) (update-user-handler req id)]
    [(list #"DELETE" (list "users" id)) (delete-user-handler req id)]

    [(list #"POST" (list "users" id "tasks")) (create-task-handler req id)]
    [(list #"GET" (list "users" id "tasks")) (list-tasks-handler req id)]

    [(list #"GET" (list "tasks" id)) (get-task-handler req id)]
    [(list #"PUT" (list "tasks" id)) (update-task-handler req id)]
    [(list #"PATCH" (list "tasks" id "done")) (complete-task-handler req id)]
    [(list #"DELETE" (list "tasks" id)) (delete-task-handler req id)]

    [_ (response/json (hasheq 'error "Not Found") #:code 404)]))

(define (start-server)
  (serve/servlet dispatch
                 #:port 8080
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 #:launch-browser? #f))
