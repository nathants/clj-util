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

(deftest test-load-conf
  (let [path (util/temp-path)
        _ (spit path (str {:a "foo"
                           :b {:c "blah"}}))
        conf (util/load-conf path)]
    (testing "keys"
      (is (= (conf :a) "foo")))
    (testing "nested keys"
      (is (= (conf :b :c) "blah")))
    (testing "nil values, like a key miss, throws an error"
      (is (thrown? AssertionError (conf :missing)))
      (is (thrown? AssertionError (conf :b :missing))))))
