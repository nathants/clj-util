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

(defn parts->path
  [parts]
  (apply str "/" (interpose "/" parts)))

(defn path->parts
  [path]
  (rest (s/split path #"/")))

(defn dirname
  [path]
  (-> path path->parts butlast parts->path))

(defn basename
  [path]
  (-> path path->parts last))

(defn mk-parent-dirs
  [path]
  (-> path java.io.File. .getParentFile .mkdirs))

(defn rm-rf
  [path]
  (-> (sh/sh "rm" "-rf" path) :exit (= 0) assert)
  (assert (-> path java.io.File. .exists not)))

(defn ls-dir
  [path]
  (->> path
       java.io.File.
       .listFiles
       (map #(.getAbsolutePath %))
       vec))
