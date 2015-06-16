(ns util
  (:require [clojure.string :as s]))


(defmacro str-format
  [& strs]
  (let [string (apply str strs)
        pattern #"\#\{([^\}]+)\}"
        parts (s/split string pattern)
        symbols (->> string (re-seq pattern) (map second) (map symbol) vec)]
    `(apply str (interleave ~parts ~(conj symbols "")))))


(defmacro for-map
  [bindings pair]
  `(->> (for ~bindings ~pair)
        (apply concat)
        (apply hash-map)))
