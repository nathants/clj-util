(ns util-test
  (:require [clojure.test :refer :all]
            [util :as util]))

(deftest test-str-format
  (let [name1 "joe"
        name2 "bob"]
    (is (= "joe" (util/str-format "#{name1}")))
    (is (= " joe" (util/str-format " #{name1}")))
    (is (= "joe " (util/str-format "#{name1} ")))
    (is (= "hey joe" (util/str-format "hey #{name1}")))
    (is (= "joe hey" (util/str-format "#{name1} hey")))
    (is (= "hey there joe and bob!"
           (util/str-format "hey there #{name1} and #{name2}!")))
    (is (= "hey there joe and bob!"
           (util/str-format "hey there #{name1}"
                            " and #{name2}!")))))
