(ns clojure-ring.handler
  (:require [clojure-ring.service :as service]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [reitit.ring.middleware.parameters :as params]
            [reitit.ring.middleware.exception :as exception]
            [muuntaja.core :as m]
            [ring.util.response :as response]))

(defn error-handler [f]
  (fn [request]
    (try
      (f request)
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (case (:type data)
            :validation (response/bad-request {:message (.getMessage e)})
            :not-found (response/not-found {:message (.getMessage e)})
            :conflict (response/status (response/response {:message (.getMessage e)}) 400)
            (response/status (response/response {:message (.getMessage e)}) 500))))
      (catch Exception e
        (response/status (response/response {:message (.getMessage e)}) 500)))))

(defn health-handler [_]
  (response/response {:status "ok" :version "1.0.0"}))

(defn create-user-handler [req]
  (let [body (:body-params req)
        user (service/create-user body)]
    (response/status (response/response user) 201)))

(defn get-all-users-handler [_]
  (response/response (service/get-all-users)))

(defn get-user-by-id-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))
        user (service/get-user-by-id id)]
    (response/response user)))

(defn update-user-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))
        body (:body-params req)
        user (service/update-user id body)]
    (response/response user)))

(defn delete-user-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))]
    (service/delete-user id)
    (response/status nil 204)))

(defn create-task-handler [req]
  (let [user-id (Integer/parseInt (-> req :path-params :id))
        body (:body-params req)
        task (service/create-task user-id body)]
    (response/status (response/response task) 201)))

(defn get-tasks-by-user-id-handler [req]
  (let [user-id (Integer/parseInt (-> req :path-params :id))
        tasks (service/get-tasks-by-user-id user-id)]
    (response/response tasks)))

(defn get-task-by-id-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))
        task (service/get-task-by-id id)]
    (response/response task)))

(defn update-task-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))
        body (:body-params req)
        task (service/update-task id body)]
    (response/response task)))

(defn mark-task-done-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))
        task (service/mark-task-done id)]
    (response/response task)))

(defn delete-task-handler [req]
  (let [id (Integer/parseInt (-> req :path-params :id))]
    (service/delete-task id)
    (response/status nil 204)))

(def app
  (ring/ring-handler
   (ring/router
    [""
     ["/health" {:get health-handler}]
     ["/users"
      {:post create-user-handler
       :get get-all-users-handler}]
     ["/users/:id"
      {:get get-user-by-id-handler
       :put update-user-handler
       :delete delete-user-handler}]
     ["/users/:id/tasks"
      {:post create-task-handler
       :get get-tasks-by-user-id-handler}]
     ["/tasks/:id"
      {:get get-task-by-id-handler
       :put update-task-handler
       :delete delete-task-handler}]
     ["/tasks/:id/done"
      {:patch mark-task-done-handler}]]
    {:data {:muuntaja m/instance
            :middleware [params/parameters-middleware
                         muuntaja/format-middleware
                         error-handler]}})
   (ring/create-default-handler)))
