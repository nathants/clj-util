(ns util-test
  (:require [clojure.test :refer :all]
            [util :refer :all]))


(deftest test-str-format
  (let [name1 "joe"
        name2 "bob"]
    (is (= "hey there joe and bob!"
           (str-format "hey there #{name1} and #{name2}!")))
    (is (= "hey there joe and bob!"
           (str-format "hey there #{name1}"
                       " and #{name2}!")))))
