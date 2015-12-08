(ns util
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(defn merge-maps
  "Merge maps respecting nested values."
  [& maps]
  (into {} (for [k (->> maps (map keys) (map set) (apply set/union))]
             [k (apply merge (map #(get % k) maps))])))

(defn keys-in
  "Return a list of all possible arguments to get-in for this map that would return non nil."
  [m]
  (->> ((fn f [k v]
          (->> (if (map? v)
                 (map #(f (cons (first %) k) (second %)) v)
                 [k ::end])))
        nil m)
       flatten
       (partition-by #(= % ::end))
       (remove #(= % [::end]))
       (map reverse)))

(defn load-confs
  "Load configuration from edn in the provided paths. The paths are searched left to right,
   so paths to the left can override values in paths to the right. You can only override values
   which are defined, so the right most path defines the base schema. Nil return values always
   throw an exception, so if you lookup a key which does not exist you will except."
  [& paths]
  (let [confs (mapv #(edn/read-string (slurp %)) paths)]
    (or (= 1 (count paths))
        (let [conf (apply load-confs (drop 1 paths))]
          (mapv #(apply conf %) (keys-in (first confs)))))
    (with-meta
      (fn [& ks]
        (doto (->> confs (map #(get-in % ks)) (remove nil?) first)
          (-> nil? not (assert (str "there is no value for keys " (vec ks) " in paths " (vec paths))))))
      {:confs confs})))

(defn load-confs-edn-str
  "Like load-confs, but takes an extra edn string which will be the left most path. This is useful
   for exposing configuration via an edn string passed as an arg to a command line interface."
  [edn-str & paths]
  (if-not (edn/read-string edn-str)
    (apply load-confs paths)
    (let [path (.getAbsolutePath (java.io.File/createTempFile "temp" ""))]
      (spit path edn-str)
      (apply load-confs (cons path paths)))))

(defmacro str-format
  [& strs]
  (let [string (apply str strs)
        pattern #"\#\{([^\}]+)\}"
        parts (s/split string pattern)
        symbols (->> string (re-seq pattern) (map second) (map symbol) vec)]
    `(apply str (interleave ~parts ~(conj symbols "")))))

(defn parts->path
  [parts]
  (apply str "/" (interpose "/" parts)))

;; todo confs this stuff works only with abs paths. should we also support rel paths?
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

(defn line-seq-cleanup
  [path]
  ((fn f [^java.io.BufferedReader reader]
     (let [line (.readLine reader)]
       (if line
         (cons line (lazy-seq (f reader)))
         (do (.close reader)
             (io/delete-file path)
             nil))))
   (io/reader path)))

(defn retry
  "Retry a fn sleeping based on millis in a seq.
  Retries until the seq is exhausted, then throws."
  [f [ms-now & ms-rest]]
  (if-let [result (try
                    (f)
                    (catch Exception ex
                      (when-not ms-now
                        (throw ex))))]
    result
    (do (Thread/sleep ms-now)
        (recur f ms-rest))))

(defn join-path
  [base & [path & paths]]
  (assert (not (re-find #"^/" (str path))) (str "path must be a relative, not: " path))
  (if path
    (apply join-path (str (s/replace (str base) #"/$" "") "/" path) paths)
    base))

(defn run
  [& args]
  (let [cmd (apply str (interpose " " args))
        res (sh/sh "bash" "-c" cmd)]
    (assert (-> res :exit (= 0)) (assoc res :cmd cmd))
    (s/trim (:out res))))

(defmacro with-tempdir
  [_ name & forms]
  `(let [~name (core/run "mktemp -d")]
     (try
       ~@forms
       (finally
         (core/run "rm -rf" ~name)))))
