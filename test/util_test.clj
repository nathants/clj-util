(ns util-test
  (:require [clojure.test :refer :all]
            [util :as util]))

(deftest test-str-format
  (let [name1 "joe"
        name2 "bob"]
    (is (= "hey there joe and bob!"
           (util/str-format "hey there #{name1} and #{name2}!")))
    (is (= "hey there joe and bob!"
           (util/str-format "hey there #{name1}"
                            " and #{name2}!")))))
