(ns clojure-ring.core
  (:require [ring.adapter.jetty :as jetty]
            [clojure-ring.handler :as handler]
            [clojure-ring.db :as db]))

(defn -main [& args]
  (println "Initializing database...")
  (db/init-db)
  (println "Starting server on port 3000...")
  (jetty/run-jetty handler/app {:port 3000 :join? true}))
