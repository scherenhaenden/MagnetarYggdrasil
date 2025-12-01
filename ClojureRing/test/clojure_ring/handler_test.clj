(ns clojure-ring.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [clojure-ring.handler :as handler]
            [clojure-ring.db :as db]
            [next.jdbc :as jdbc]
            [clojure.java.io :as io]))

(def test-db-file "test_magnetar.db")

(def test-db-spec {:dbtype "sqlite" :dbname test-db-file})
(def test-datasource (jdbc/get-datasource test-db-spec))

(defn setup-db [f]
  ;; Clean up old test db
  (io/delete-file test-db-file true)
  (with-redefs [db/datasource test-datasource]
    (db/init-db)
    (f))
  (io/delete-file test-db-file true))

(use-fixtures :each setup-db)

(deftest health-check
  (let [response (handler/app (mock/request :get "/health"))]
    (is (= (:status response) 200))
    (is (= "application/json; charset=utf-8" (get-in response [:headers "Content-Type"])))))

(deftest user-flow
  (testing "Create user"
    (let [response (handler/app (-> (mock/request :post "/users")
                                    (mock/json-body {:username "integration" :email "int@test.com"})))]
      (is (= 201 (:status response)))))

  (testing "Get users"
    (let [response (handler/app (mock/request :get "/users"))]
      (is (= 200 (:status response)))))

  (testing "Get user by id"
    ;; First create
    (handler/app (-> (mock/request :post "/users")
                     (mock/json-body {:username "u1" :email "u1@test.com"})))
    (let [response (handler/app (mock/request :get "/users/1"))]
      (is (= 200 (:status response))))))
