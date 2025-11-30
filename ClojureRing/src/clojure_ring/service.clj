(ns clojure-ring.service
  (:require [clojure-ring.repository :as repo]))

(defn create-user [user]
  (if (or (empty? (:username user)) (empty? (:email user)))
    (throw (ex-info "Invalid user data" {:type :validation}))
    (try
      (repo/create-user user)
      (catch Exception e
        (if (or (.contains (.getMessage e) "UNIQUE constraint failed")
                (.contains (.getMessage e) "SQLITE_CONSTRAINT"))
          (throw (ex-info "User already exists" {:type :conflict}))
          (throw e))))))

(defn get-all-users []
  (repo/get-all-users))

(defn get-user-by-id [id]
  (let [user (repo/get-user-by-id id)]
    (if user
      user
      (throw (ex-info "User not found" {:type :not-found})))))

(defn update-user [id user]
  (if (and (nil? (:username user)) (nil? (:email user)))
    (throw (ex-info "Invalid user data" {:type :validation}))
    (let [updated-user (repo/update-user id user)]
      (if updated-user
        updated-user
        (throw (ex-info "User not found" {:type :not-found}))))))

(defn delete-user [id]
  (repo/delete-user id))

(defn create-task [user-id task]
  (if (or (empty? (:title task)) (empty? (:description task)))
    (throw (ex-info "Invalid task data" {:type :validation}))
    ;; Ensure user exists first
    (if (repo/get-user-by-id user-id)
      (repo/create-task user-id task)
      (throw (ex-info "User not found" {:type :not-found})))))

(defn get-tasks-by-user-id [user-id]
  (if (repo/get-user-by-id user-id)
    (repo/get-tasks-by-user-id user-id)
    (throw (ex-info "User not found" {:type :not-found}))))

(defn get-task-by-id [id]
  (let [task (repo/get-task-by-id id)]
    (if task
      task
      (throw (ex-info "Task not found" {:type :not-found})))))

(defn update-task [id task]
  (if (and (nil? (:title task)) (nil? (:description task)))
     ;; Allow partial updates, but if body is completely empty maybe we should just return existing?
     ;; Spec says input: { "title": "...", "description": "..." } implies full or partial?
     ;; "Partial updates allowed" usually means at least one field.
     ;; If both missing, it's a no-op or invalid?
     ;; Let's assume at least one is required for a meaningful update, OR just let repository handle no-op.
     ;; My repository handle no-op returns existing task.
     ;; But checking for emptiness might be good.
     ;; Let's allow empty if the user just sends {} it returns the task.
    (repo/update-task id task)
    (let [updated-task (repo/update-task id task)]
      (if updated-task
        updated-task
        (throw (ex-info "Task not found" {:type :not-found}))))))

(defn mark-task-done [id]
  (let [updated-task (repo/mark-task-done id)]
    (if updated-task
      updated-task
      (throw (ex-info "Task not found" {:type :not-found})))))

(defn delete-task [id]
  (repo/delete-task id))
