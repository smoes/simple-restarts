(ns simple-restarts.core-test
  (:require [clojure.test :refer :all]
            [simple-restarts.core :refer :all]))

(defcondition my-test-condition [arg])
(defcondition my-test-condition-2 [arg])

(deftest handler-binding-test
  (handler-bind
    [my-test-condition   (fn [a] (invoke-restart :restart a))
     my-test-condition-2 (fn [a] (invoke-restart :restart (* 2 a)))]

    (is (get *handlers* my-test-condition))
    (is (= ((get *handlers* my-test-condition) 3) (invoke-restart :restart 3)))

    (is (get *handlers* my-test-condition-2))
    (is (= ((get *handlers* my-test-condition-2) 3) (invoke-restart :restart 6)))))


(deftest fire-condition-test
  (is (thrown? Exception (fire-condition (my-test-condition "bla")))))


(deftest restart-case-test
  (handler-bind
    [my-test-condition   (fn [a] (invoke-restart :restart a))
     my-test-condition-2 (fn [a] (invoke-restart :restart (* 2 a)))]

    (is (= 4 (restart-case
               (fire-condition (my-test-condition 2))
               (restart :restart (fn [a] (* a 2))))))

    (is (= 8 (restart-case
               (fire-condition (my-test-condition-2 2))
               (restart :restart (fn [a] (* a 2))))))))

(deftest nested-restart-cases-test
  (handler-bind
    [my-test-condition   (fn [a] (invoke-restart :restart a))
     my-test-condition-2 (fn [a] (invoke-restart :restart2 (* 2 a)))]

    (is (= 4 (restart-case
               (restart-case
                 (fire-condition (my-test-condition 2))
                 (restart :restart2 (fn [a] a)))

               (restart :restart (fn [a] (* a 2))))))

    (is (= 4 (restart-case
               (restart-case
                 (fire-condition (my-test-condition-2 2))
                 (restart :restart2 (fn [a] a)))

               (restart :restart (fn [a] (* a 2))))))))
