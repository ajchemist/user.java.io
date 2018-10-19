(ns user.java.io.alpha-test
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [clojure.java.io :as jio]
   [user.java.io.alpha :refer :all]
   ))


(deftest main


  (is
    (identical?
      :test
      (let [dest-dir (mktempdir "test")]
        (do-operations
          dest-dir
          [{:op       :write
            :path     "testfile"
            :write-fn (fn [os] (jio/copy (jio/input-stream (.getBytes (pr-str :test))) os))}])
        (read-string (slurp (jio/file dest-dir "testfile"))))))


  )
