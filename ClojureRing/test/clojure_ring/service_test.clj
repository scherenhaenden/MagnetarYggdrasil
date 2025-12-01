(ns clojure-ring.service-test
  (:require [clojure.test :refer :all]
            [clojure-ring.service :as service]
            [clojure-ring.repository :as repo]))

(deftest create-user-test
  (testing "Valid user creation"
    (with-redefs [repo/create-user (fn [_] {:id 1 :username "test" :email "test@test.com"})]
      (is (= {:id 1 :username "test" :email "test@test.com"}
             (service/create-user {:username "test" :email "test@test.com"})))))

  (testing "Invalid user creation"
    (is (thrown? clojure.lang.ExceptionInfo (service/create-user {:username "" :email ""}))))

  (testing "Duplicate user creation"
    (with-redefs [repo/create-user (fn [_] (throw (java.sql.SQLException. "UNIQUE constraint failed")))]
      (is (thrown? clojure.lang.ExceptionInfo (service/create-user {:username "test" :email "test@test.com"}))))))

(deftest get-user-test
  (testing "Get existing user"
    (with-redefs [repo/get-user-by-id (fn [_] {:id 1 :username "test"})]
      (is (= {:id 1 :username "test"} (service/get-user-by-id 1)))))

  (testing "Get non-existing user"
    (with-redefs [repo/get-user-by-id (fn [_] nil)]
      (is (thrown? clojure.lang.ExceptionInfo (service/get-user-by-id 999))))))
