#lang racket

(provide (struct-out user)
         (struct-out task))

(struct user (id username email) #:transparent)
(struct task (id user_id title description status) #:transparent)
