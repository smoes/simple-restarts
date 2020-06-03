(ns simple-restarts.core-test
  (:require [clojure.test :refer :all]
            [simple-restarts.core :refer :all]))

(defcondition my-test-condition [line])
(defcondition my-test-condition-2 [line])

(deftest handler-binding
  (handler-bind
    [my-test-condition   (fn [a] (invoke-restart :restart a))
     my-test-condition-2 (fn [a] (invoke-restart :restart (* 2 a)))]

    (is (get *handlers* my-test-condition))
    (is (= ((get *handlers* my-test-condition) 3) (invoke-restart :restart 3)))

    (is (get *handlers* my-test-condition-2))
    (is (= ((get *handlers* my-test-condition-2) 3) (invoke-restart :restart 6)))))
