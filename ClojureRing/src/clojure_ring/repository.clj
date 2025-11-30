(ns clojure-ring.repository
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql :as sql]
            [clojure-ring.db :as db]
            [clojure.string :as str]))

(defn create-user [user]
  (jdbc/execute-one! db/datasource
                     ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING *"
                      (:username user) (:email user)]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn get-all-users []
  (jdbc/execute! db/datasource
                 ["SELECT * FROM users"]
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn get-user-by-id [id]
  (jdbc/execute-one! db/datasource
                     ["SELECT * FROM users WHERE id = ?" id]
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn update-user [id user]
  (let [clauses (cond-> []
                  (:username user) (conj "username = ?")
                  (:email user) (conj "email = ?"))
        params (cond-> []
                 (:username user) (conj (:username user))
                 (:email user) (conj (:email user)))
        sql (str "UPDATE users SET " (str/join ", " clauses) " WHERE id = ? RETURNING *")]
    (if (empty? clauses)
      (get-user-by-id id) ;; No changes
      (jdbc/execute-one! db/datasource
                         (into [sql] (conj params id))
                         {:builder-fn rs/as-unqualified-lower-maps}))))

(defn delete-user [id]
  (jdbc/execute-one! db/datasource
                     ["DELETE FROM users WHERE id = ?" id]))

(defn create-task [user-id task]
  (let [result (jdbc/execute-one! db/datasource
                                  ["INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0) RETURNING *"
                                   user-id (:title task) (:description task)]
                                  {:builder-fn rs/as-unqualified-lower-maps})]
    (update result :done #(if (= 1 %) true false))))

(defn get-tasks-by-user-id [user-id]
  (let [results (jdbc/execute! db/datasource
                               ["SELECT * FROM tasks WHERE user_id = ?" user-id]
                               {:builder-fn rs/as-unqualified-lower-maps})]
    (map #(update % :done (fn [v] (if (= 1 v) true false))) results)))

(defn get-task-by-id [id]
  (let [result (jdbc/execute-one! db/datasource
                                  ["SELECT * FROM tasks WHERE id = ?" id]
                                  {:builder-fn rs/as-unqualified-lower-maps})]
    (if result
      (update result :done #(if (= 1 %) true false))
      nil)))

(defn update-task [id task]
  (let [clauses (cond-> []
                  (:title task) (conj "title = ?")
                  (:description task) (conj "description = ?"))
        params (cond-> []
                 (:title task) (conj (:title task))
                 (:description task) (conj (:description task)))
        sql (str "UPDATE tasks SET " (str/join ", " clauses) " WHERE id = ? RETURNING *")]
    (if (empty? clauses)
      (get-task-by-id id)
      (let [result (jdbc/execute-one! db/datasource
                                      (into [sql] (conj params id))
                                      {:builder-fn rs/as-unqualified-lower-maps})]
        (if result
          (update result :done #(if (= 1 %) true false))
          nil)))))

(defn mark-task-done [id]
  (let [result (jdbc/execute-one! db/datasource
                                  ["UPDATE tasks SET done = 1 WHERE id = ? RETURNING *" id]
                                  {:builder-fn rs/as-unqualified-lower-maps})]
    (if result
      (update result :done #(if (= 1 %) true false))
      nil)))

(defn delete-task [id]
  (jdbc/execute-one! db/datasource
                     ["DELETE FROM tasks WHERE id = ?" id]))
