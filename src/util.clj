(ns util
  (:require [clojure.string :as s]
            [me.raynes.fs :as fs]
            [clojure.edn :as edn]
            [taoensso.timbre :as timbre]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.set :as set]
            [me.raynes.conch.low-level :as csh]))

(defmacro str-format
  [& strs]
  (let [string (apply str strs)
        pattern #"\#\{([^\}]+)\}"
        symbols (->> string (re-seq pattern) (map second) (map symbol) vec)
        parts (take (inc (count symbols)) (concat (s/split string pattern) (repeat "")))]
    `(apply str ~(vec (interleave parts (conj symbols ""))))))

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

(defn drop-trailing-slash
  [path]
  (s/replace path #"/$" ""))

(defn ensure-trailing-slash
  [path]
  (str (drop-trailing-slash path) "/"))

(defn dirname
  [path]
  (s/replace path #"/.+$" ""))

(defn basename
  [path]
  (-> path
    (s/replace #"/$" "")
    (s/replace #".*/(.+)$" "$1")))

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

(defn line-seq-closing
  "a line-seq that closes it's file when exhausted."
  [path]
  ((fn f [^java.io.BufferedReader reader]
     (let [line (.readLine reader)]
       (if line
         (cons line (lazy-seq (f reader)))
         (.close reader))))
   (clojure.java.io/reader path)))

(defn line-seq-cleanup
  "a line-seq that closes and deletes it's file when exhausted."
  [path]
  ((fn f [^java.io.BufferedReader reader]
     (let [line (.readLine reader)]
       (if line
         (cons line (lazy-seq (f reader)))
         (do (.close reader)
             (io/delete-file path)
             nil))))
   (io/reader path)))

(defn retries
  [number & [exponent jitter]]
  (->> (range number)
    (map inc)
    (map #(Math/pow % (or exponent 1.1)))
    (map #(* ^double % 1000))
    (map #(* ^double % ^double (+ 1 (* (or jitter 0.25) (rand)))))
    (map long)))

(defn retry
  "Retry a fn sleeping based on millis in a seq.
  Retries until the seq is exhausted, then throws.
  ie (retry f (retries 10))"
  [f [ms-now & ms-rest]]
  (let [[status res] (try
                       [::success (f)]
                       (catch Throwable ex
                         [::fail ex]))]
    (condp = status
      ::fail (if-not ms-now
               (throw res)
               (do (Thread/sleep ms-now)
                   (recur f ms-rest)))
      ::success res)))

(defn join-path
  [base & [path & paths]]
  (assert (not (re-find #"^/" (str path))) (str "path must be a relative, not: " path))
  (if path
    (apply join-path (str (s/replace (str base) #"/$" "") "/" path) paths)
    base))


;; TODO merge stderr/stdout
(defn run-stream
  [& cmds]
  (let [cmd (s/join " " (map str cmds))
        _ (println :cmd cmd)
        proc (me.raynes.conch.low-level/proc "bash" "-c" cmd)
        out-seq (-> proc :out clojure.java.io/reader line-seq)
        err-seq (-> proc :err clojure.java.io/reader line-seq)
        _ (future (dorun (map println out-seq)))
        _ (dorun (map println out-seq))]
    (assert (-> proc :process .waitFor zero?) (str "cmd failed to exit 0: " cmd))
    (s/join "\n" out-seq)))

(defn run [& args]
  (let [[in cmd] (if (= :in (first args))
                   [(second args) (apply str (interpose " " (drop 2 args)))]
                   [nil (apply str (interpose " " args))])
        res (if in
              (clojure.java.shell/sh "bash" "-c" cmd :in in)
              (clojure.java.shell/sh "bash" "-c" cmd))]
    (assert (-> res :exit (= 0)) (assoc res :cmd cmd))
    (s/trim (:out res))))

(defn run-raw [& args]
  (let [[in args] (if (= :in (first args))
                    [(second args) (drop 2 args)]
                    [nil args])
        res (if in
              (apply clojure.java.shell/sh (concat args [:in in]))
              (apply clojure.java.shell/sh args))]
    (assert (-> res :exit (= 0)) (assoc res :cmd args))
    (s/trim (:out res))))

(defmacro with-tempdir
  [_ name & forms]
  `(let [~name (core/run "mktemp -d")]
     (try
       ~@forms
       (finally
         (core/run "rm -rf" ~name)))))

(defn mkdir-spit
  [path content & opts]
  (fs/mkdirs (fs/parent path))
  (apply spit path content opts))

(defn indent
  [n x]
  (->> x
    s/split-lines
    (map #(str (apply str (repeat n " ")) %))
    (s/join "\n")))

(defn parse-s3-url
  [s3-url]
  (let [[_ bucket path] (re-find #"s3://([^/]+)/(.+)?" s3-url)]
    (assert bucket (str "bad s3-url, should be s3://bucket/path/..., not: " s3-url))
    [bucket (or path "")]))

(defn s3-bucket
  [s3-url]
  (first (parse-s3-url s3-url)))

(defn s3-path
  [s3-url]
  (second (parse-s3-url s3-url)))

(defn -cache-dir
  []
  (doto (str (System/getProperty "user.home") "/tmp")
    (-> java.io.File. .mkdirs)))

(defn -cache-path
  [x]
  (str (-cache-dir) "/disk_cache_" (hash x)))

(defn memoize-disk
  [f]
  (fn [& args]
    (let [path (-cache-path (conj args (str f)))]
      (if (-> path java.io.File. .exists)
        (edn/read-string (slurp path))
        (let [res (apply f args)]
          (spit path (pr-str res))
          res)))))

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (timbre/error ex "Uncaught exception in" (.getName thread)))))
