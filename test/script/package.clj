(ns script.package
  (:require
   [clojure.java.io :as jio]
   [clojure.tools.deps.alpha :as deps]
   [clojure.tools.deps.alpha.reader :as deps.reader]
   [clojure.tools.deps.alpha.extensions :as deps.ext]
   [clojure.tools.deps.alpha.extensions.git :as deps.git]
   [clojure.tools.namespace.find :as ns.find]
   [user.tools.deps.maven.alpha :as maven]
   [user.tools.deps.clean :as clean]
   [user.tools.deps.compile :as compile]
   [user.tools.deps.jar :as jar]
   [user.tools.deps.install :as install]
   [script.time]
   [user.java.io.alpha :as io]
   )
  (:import
   java.nio.file.Path
   ))


(set! *warn-on-reflection* true)


(def ^Path target-path (io/path "target"))
(def ^Path classes-path (. target-path resolve "classes"))


(defn package
  []
  (time
    (do
      (clean/clean target-path)
      ;; (compile/compile (ns.find/find-namespaces (map jio/file ["src"])) nil nil {:direct-linking true})
      (let [pom-path (maven/sync-pom 'user.java.io {:mvn/version (script.time/chrono-version-str)})
            jarpath  (jar/jar nil nil nil
                              {:pom-path pom-path
                               :out-path (. target-path resolve "package.jar")
                               ;; :compile-path classes-path
                               })]
        (println (str (install/install nil nil jarpath pom-path)))))))


(defn -main
  [& xs]
  (try
    (package)
    (System/exit 0)
    (finally
      (shutdown-agents))))


(set! *warn-on-reflection* false)


(comment
  (package)
  )
