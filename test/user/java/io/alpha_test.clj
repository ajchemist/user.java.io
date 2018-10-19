(ns user.java.io.alpha-test
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [clojure.java.io :as jio]
   [user.java.io.alpha :refer :all]
   ))


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
