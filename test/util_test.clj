(ns util-test
  (:require [clojure.test :refer :all]
            [util :as util]))

(deftest test-merge-maps
  (is (= {:a {:b 1 :c 2}}
         (util/merge-maps
          {:a {:b 1}}
          {:a {:c 2}}))))

(defn temp-path
  []
  (.getAbsolutePath (java.io.File/createTempFile "temp" "")))

(deftest test-str-format
  (let [name1 "joe"
        name2 "bob"]
    (is (= "hey there joe and bob!"
           (util/str-format "hey there #{name1} and #{name2}!")))
    (is (= "hey there joe and bob!"
           (util/str-format "hey there #{name1}"
                            " and #{name2}!")))))

(deftest load-confs-with-nothing
  (let [conf (util/load-confs)]
    (is (thrown? AssertionError (conf :no-keys-in-empty-conf)))))

(deftest test-load-confs
  (let [path (temp-path)
        data {:a "foo"
              :b {:c "blah"}
              :d nil
              :e false}
        _ (spit path (str data))
        conf (util/load-confs path)]
    (testing "nil is illegal as a value"
      (is (thrown? AssertionError (conf :d))))
    (testing "false is fine as a value"
      (is (= false (conf :e))))
    (testing "keys"
      (is (= (conf :a) "foo")))
    (testing "nested keys"
      (is (= (conf :b :c) "blah")))
    (testing "nil values, like a key miss, throws an error"
      (is (thrown? AssertionError (conf :missing)))
      (is (thrown? AssertionError (conf :b :missing))))
    (testing "conf data is attached to meta"
      (is (= [data] (-> conf meta :confs))))))

(deftest test-load-confs-edn-str
  (let [path (temp-path)
        data {:a :default-val}]
    (spit path (str data))
    (testing "overriding a value from an edn string"
      (let [conf (util/load-confs "{:a :new-val}" path)]
        (is (= :new-val (conf :a)))))
    (testing "edn strings which read to nil are ignored"
      (let [conf (util/load-confs "   " path)]
        (is (= :default-val (conf :a)))))))

(deftest test-load-confs-multiple-paths
  (let [extra-conf (temp-path)
        base-conf (temp-path)]
    (testing "confs provided are searched left to right for values"
      (spit base-conf (str {:a :default-val}))
      (spit extra-conf (str {:a :new-val}))
      (let [conf (util/load-confs extra-conf base-conf)]
        (is (= :new-val (conf :a)))))
    (testing "left most value can be an edn-str"
      (spit base-conf (str {:a :default-val}))
      (spit extra-conf (str {:a :new-val}))
      (let [conf (util/load-confs "{:a :newest-val}" extra-conf base-conf)]
        (is (= :newest-val (conf :a)))))
    (testing "extra confs on the left must be overriding values that are already defined in confs on the right"
      (spit extra-conf (str {:unknown-key :not-gonna-work}))
      (is (thrown? AssertionError (util/load-confs extra-conf base-conf))))))

(deftest test-keys-in
  (is (= [[1 2]
          [1 4]
          [1 6 7]]
         (util/-keys-in {1 {2 :val
                            4 :val
                            6 {7 :val}}}))))
