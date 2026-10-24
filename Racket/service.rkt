#lang racket

(require "repository.rkt"
         "models.rkt")

(provide (all-defined-out))

(define (create-user-service username email)
  (create-user username email))

(define (list-users-service)
  (get-all-users))

(define (get-user-service id)
  (get-user-by-id id))

(define (update-user-service id username email)
  (update-user id username email))

(define (delete-user-service id)
  (delete-user id))

(define (create-task-service user-id title description)
  (create-task user-id title description))

(define (list-user-tasks-service user-id)
  (get-tasks-by-user-id user-id))

(define (get-task-service id)
  (get-task-by-id id))

(define (update-task-service id title description)
  (update-task id title description))

(define (complete-task-service id)
  (mark-task-done id))

(define (delete-task-service id)
  (delete-task id))
