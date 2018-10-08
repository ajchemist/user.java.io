(ns user.java.io.alpha
  (:refer-clojure :exclude [name])
  (:require
   [clojure.java.io :as jio]
   )
  (:import
   java.io.File
   java.net.URI
   java.net.URL
   java.nio.file.CopyOption
   java.nio.file.FileSystem
   java.nio.file.FileSystems
   java.nio.file.FileVisitOption
   java.nio.file.FileVisitResult
   java.nio.file.FileVisitor
   java.nio.file.Files
   java.nio.file.LinkOption
   java.nio.file.OpenOption
   java.nio.file.Path
   java.nio.file.Paths
   java.nio.file.SimpleFileVisitor
   java.nio.file.StandardCopyOption
   java.nio.file.StandardOpenOption
   java.nio.file.attribute.FileAttribute
   java.nio.file.attribute.FileTime
   java.nio.file.attribute.PosixFilePermission
   java.nio.file.attribute.PosixFilePermissions
   ))


(set! *warn-on-reflection* true)


;; * protocol


(defprotocol IPath
  (^Path path [x]))


(defprotocol FilenameUtils
  (^String filename [this])
  (^String filepath [this])
  (^String parent [this])
  (^File parent-file [this])
  (^String basename [this])
  (^String extension [this]))


(defprotocol UriUtils
  (^URI to-uri [x]))


(def default-uri-utils-interface-impl
  {:to-url (fn [x] (throw (IllegalArgumentException. (str "Cannot coerce <" (pr-str x) "> to URI"))))})


(defprotocol UrlUtils
  (^URL to-url [x])
  (^String to-external-form [url]))


(def default-url-utils-interface-impl
  {:to-url           (fn [x] (throw (IllegalArgumentException. (str "Cannot coerce <" (pr-str x) "> to URL"))))
   :to-external-form (fn [x] (to-external-form (to-url x)))})


;; * nio.files


;; ** resolution


;; ** options


(def ^:dynamic ^"[Ljava.nio.file.LinkOption;" *no-follow* (LinkOption/values))


(def ^"[Ljava.nio.file.LinkOption;" default-link-options (make-array LinkOption 0))


(defn- interpret-open-opt
  [opt]
  (case opt
    :append     StandardOpenOption/APPEND
    :create     StandardOpenOption/CREATE
    :create-new StandardOpenOption/CREATE_NEW
    :delete     StandardOpenOption/DELETE_ON_CLOSE
    :dsync      StandardOpenOption/DSYNC
    :read       StandardOpenOption/READ
    :sparse     StandardOpenOption/SPARSE
    :sync       StandardOpenOption/SYNC
    :truncate   StandardOpenOption/TRUNCATE_EXISTING
    :write      StandardOpenOption/WRITE
    (throw (IllegalArgumentException. (str "Illegal open option: " opt)))))


(defn- interpret-copy-opt
  [opt]
  (case opt
    :atomic          StandardCopyOption/ATOMIC_MOVE
    :replace         StandardCopyOption/REPLACE_EXISTING
    :copy-attributes StandardCopyOption/COPY_ATTRIBUTES
    (throw (IllegalArgumentException. (str "Illegal copy option: " opt)))))


(defn- ^"[Ljava.nio.file.OpenOption;"
  interpret-open-opts
  [opts]
  (into-array OpenOption (map interpret-open-opt opts)))


(defn- ^"[Ljava.nio.file.CopyOption;"
  interpret-copy-opts
  [opts]
  (into-array CopyOption (map interpret-copy-opt opts)))


;; ** predicates


(defn ^Boolean exists?
  [x]
  (Files/exists (path x) *no-follow*))


(defn ^Boolean file?
  [x]
  (Files/isRegularFile (path x) *no-follow*))


(defn ^Boolean directory?
  [x]
  (Files/isDirectory (path x) *no-follow*))


(defn same-directory?
  [path1 path2]
  (let [normalized-path1 (.. (path path1) toAbsolutePath normalize)
        normalized-path2 (.. (path path2) toAbsolutePath normalize)]
    (.equals normalized-path2 normalized-path1)))


(defn parent-path?
  "Return true if `path1` is an ancestor path of `path2`"
  [path1 path2]
  (let [normalized-path1 (.. (path path1) toAbsolutePath normalize)
        normalized-path2 (.. (path path2) toAbsolutePath normalize)]
    (and
      (not (.equals normalized-path2 normalized-path1))
      (.startsWith normalized-path2 normalized-path1))))


;; ** operations


(defn ^Path mkdir
  [dirpath]
  (Files/createDirectories (path dirpath) (make-array FileAttribute 0)))


(defn ^Path mkparents
  [target]
  (when-let [parent (.getParent (path target))]
    (mkdir parent)))


(defn mktempdir
  ([^String prefix]
   (Files/createTempDirectory prefix (make-array FileAttribute 0)))
  ([^String dir ^String prefix]
   (Files/createTempDirectory (mkdir (path dir)) prefix (make-array FileAttribute 0))))


(defn move!
  "Move or rename a file to a target file.
  By default, this method attempts to move the file to the target
  file, failing if the target file exists except if the source and
  target are the same file, in which case this method has no
  effect. If the file is a symbolic link then the symbolic link
  itself, not the target of the link, is moved.
  This method may be invoked to move an empty directory. When invoked
  to move a directory that is not empty then the directory is moved if
  it does not require moving the entries in the directory. For
  example, renaming a directory on the same FileStore will usually not
  require moving the entries in the directory. When moving a directory
  requires that its entries be moved then this method fails (by
  throwing an IOException)."
  ([src dst]
   (move! src dst #{:atomic :replace}))
  ([src dst flags]
   (let [opts (interpret-copy-opts flags)]
     (Files/move (path src) (path dst) opts))))


(defn copy!
  ([src dst]
   (copy! src dst nil))
  ([src dst attrs]
   (copy! src dst attrs #{:replace}))
  ([src dst {:keys [^FileTime time ^java.util.Set mode]} flags]
   (let [src  (path src)
         dst  (path dst)
         opts (interpret-copy-opts flags)]
     (Files/copy src dst opts)
     (when time (Files/setLastModifiedTime dst time))
     (when mode (Files/setPosixFilePermissions dst mode)))))


(defn write!
  ([target writer-fn]
   (write! target writer-fn #{:create}))
  ([target writer-fn flags]
   (let [^Path target (doto (path target) (mkparents))
         opts         (interpret-open-opts flags)]
     (with-open [os (Files/newOutputStream target opts)]
       (writer-fn os)))))



(defn resource-ext-forms
  {:deprecated true}
  ([paths]
   (resource-ext-forms [] paths))
  ([coll paths]
   (into coll
     (comp
      (filter string?)
      (map jio/resource)
      (filter some?)
      (map (fn [^URL url] (. url toExternalForm))))
     paths)))


(defn file-ext-forms
  {:deprecated true}
  ([paths]
   (file-ext-forms [] paths))
  ([coll paths]
   (into coll
     (comp
      (filter string?)
      (map jio/file)
      (filter
       (fn [^File file] (. file isFile)))
      (map
       (fn [^File file]
         (.. file toURI toURL toExternalForm))))
     paths)))


(defn dirwalk-1
  "return lazy sequence"
  {:style/indent [:defn]}
  [dirpath pattern]
  (->> dirpath
    jio/file .listFiles seq
    (filter #(re-matches pattern (memfn ^File getName)))))


(defn dirwalk
  "return lazy sequence"
  {:style/indent [:defn]}
  [dirpath pattern]
  (->> dirpath
    jio/file file-seq
    (filter #(re-matches pattern (memfn ^File getName)))))


;; * filename utils internal


;; ** str


(defn get-basename
  [^String filename]
  (if (pos? (. filename indexOf "."))
    (subs filename 0 (. filename lastIndexOf "."))
    filename))


(defn get-extension
  [^String filename]
  (if (pos? (. filename indexOf "."))
    (subs filename (. filename lastIndexOf "."))
    ""))


;; ** file


(defn ^String file-basename
  [^File file]
  (get-basename (. file getName)))


(defn ^String file-extension
  [^File file]
  (get-extension (. file getName)))


;; ** url


(defn ^String url-filename
  [^URL url]
  (let [path (. url getPath)]
    (subs path (unchecked-inc (. path lastIndexOf "/")))))


(defn ^String url-basename
  [^URL url]
  (get-basename (url-filename url)))


(defn ^String url-extension
  [^URL url]
  (get-extension (url-filename url)))


;; * extend


(extend-type Path
  IPath
  (path [x] x))


(extend-type FileSystem
  IPath
  (path [x] (first (.getRootDirectories x))))


(extend-type String
  IPath
  (path [x] (Paths/get x (make-array String 0)))

  FilenameUtils
  (filename [this] (.getName (jio/file this)))
  (filepath [this] (.getPath (jio/file this)))
  (parent [this] (.getParent (jio/file this)))
  (parent-file [this] (.getParentFile (jio/file this)))
  (basename [this] (get-basename this))
  (extension [this] (get-extension this)))


(extend java.io.File
  IPath
  {:path (fn [^File x] (.toPath x))}

  FilenameUtils
  {:filename    (fn [^File x] (.getName x))
   :filepath    (fn [^File x] (.getPath x))
   :parent      (fn [^File x] (.getParent x))
   :parent-file (fn [^File x] (.getParentFile x))
   :basename    (fn [^File x] (file-basename x))
   :extension   (fn [^File x] (file-extension x))}

  UriUtils
  {:to-uri (fn [^File x] (. x toURI))}

  UrlUtils
  {:to-url           (fn [^File x] (.. x toURI toURL))
   :to-external-form (fn [^File x] (.. x toURI toURL toExternalForm))})


(extend-type java.net.URI
  IPath
  (path [x] (Paths/get x))

  UriUtils
  (to-uri [x] x)

  UrlUtils
  (to-url [x] (. x toURL))
  (to-external-form [x] (.. x toURL toExternalForm)))


(extend-type java.net.URL
  FilenameUtils
  (filename [this] (url-filename this))
  (filepath [this] (. this getPath))
  (basename [this] (url-basename this))
  (extension [this] (url-extension this))

  UriUtils
  (to-uri [this] (. this toURI))

  UrlUtils
  (to-url [x] x)
  (to-external-form [x] (. x toExternalForm)))


(set! *warn-on-reflection* false)
