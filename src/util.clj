(ns util
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]))

(defn temp-path
  []
  (.getAbsolutePath (java.io.File/createTempFile "temp" "")))

(defn load-conf
  [path]
  (let [conf (edn/read-string (slurp path))]
    (with-meta
      (fn [& ks]
        (doto (get-in conf ks)
          (-> nil? not (assert (str "there is no value for keys " (vec ks) " in config \"" path "\"")))))
      {:data conf})))

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

;; todo all this stuff works only with abs paths. should we also support rel paths?
(defn path->parts
  [path]
  (remove s/blank? (s/split path #"/")))

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
  [path & {:keys [hidden-files]}]
  (->> path
       java.io.File.
       .listFiles
       (map #(.getAbsolutePath %))
       (filter #(->> % basename (re-find #"^\.") not (or hidden-files)))
       vec))

(defn path-exists
  [path]
  (-> path java.io.File. .exists))
