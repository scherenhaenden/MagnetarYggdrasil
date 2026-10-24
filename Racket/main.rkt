#lang racket

(require "db.rkt"
         "handlers.rkt")

(init-db)
(printf "Starting server on port 8080...\n")
(start-server)
