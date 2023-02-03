(ns user.java.io.alpha-test
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [user.java.io.alpha :refer :all]
   )
  (:import
   java.net.URI
   java.nio.file.FileSystem
   java.nio.file.FileSystems
   java.util.HashMap
   ))


(defn mkjaruri
  ^URI
  [jarpath]
  (URI/create (str "jar:" (.. (as-path jarpath) toAbsolutePath normalize toUri))))


(defn mkjarfs
  ^FileSystem
  ([jarpath]
   (mkjarfs jarpath nil))
  ([jarpath {:keys [create encoding]}]
   (let [jaruri (mkjaruri jarpath)
         env    (HashMap.)]
     (when create
       ;; (jio/make-parents jarpath)
       (mkparents jarpath)
       (.put env "create" (str (boolean create))))
     (when encoding (.put env "encoding" (str encoding)))
     (FileSystems/newFileSystem jaruri env))))


(defn getjarfs
  ^FileSystem
  [jarpath]
  (let [jaruri (mkjaruri jarpath)]
    (if (file? jarpath)
      (try
        (FileSystems/getFileSystem jaruri)
        (catch java.nio.file.FileSystemNotFoundException _
          (mkjarfs jarpath)))
      (mkjarfs jarpath {:create true}))))


(deftest main
  (let [dest-dir (mktempdir "test")]
    (do-operations
      dest-dir
      [{:op       :write
        :path     "testfile"
        :write-fn (fn [os] (jio/copy (jio/input-stream (.getBytes (pr-str :test))) os))}
       {:op       :copy
        :src      "deps.edn"
        :path     "deps.edn"}])
    (is (identical? :test (read-string (slurp (jio/file dest-dir "testfile")))))
    (is (file? (path-resolve dest-dir "deps.edn"))))
  )


(comment
  (defn get-jarpath
    ^String
    [lib coord]
    (let [path (get-in (deps/resolve-deps {:deps {lib coord}} {}) [lib :paths 0])]
      (when (str/ends-with? path ".jar")
        path)))


  (paths-copy-operations ["src" "test"])


  (transduce-file-tree
    (map
      (fn [[path]]
        (println path)
        (str path)))
    conj!
    []
    #_(getjarfs (get-jarpath 'user.java.io {:mvn/version "2018.292.70200"}))
    (getjarfs (get-jarpath 'org.clojure/tools.deps.alpha {:mvn/version "0.5.460"})))
  )
