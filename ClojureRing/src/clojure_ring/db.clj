(ns clojure-ring.db
  (:require [next.jdbc :as jdbc]))

(def db-spec
  {:dbtype "sqlite"
   :dbname "magnetar.db"})

(def datasource (jdbc/get-datasource db-spec))

(defn init-db []
  (jdbc/execute! datasource ["PRAGMA foreign_keys = ON;"])
  (jdbc/execute! datasource
    ["CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      email TEXT UNIQUE NOT NULL
    )"])
  (jdbc/execute! datasource
    ["CREATE TABLE IF NOT EXISTS tasks (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      title TEXT NOT NULL,
      description TEXT NOT NULL,
      done INTEGER DEFAULT 0 CHECK(done IN (0, 1)),
      FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
    )"]))
