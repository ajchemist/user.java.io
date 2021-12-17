(ns user.java.io.alpha
  (:refer-clojure :exclude [name])
  (:require
   [clojure.java.io :as jio]
   )
  (:import
   java.io.File
   java.io.InputStream
   java.io.OutputStream
   java.net.URL
   java.nio.charset.Charset
   java.nio.file.CopyOption
   java.nio.file.FileSystem
   java.nio.file.FileVisitOption
   java.nio.file.FileVisitResult
   java.nio.file.FileVisitor
   java.nio.file.Files
   java.nio.file.LinkOption
   java.nio.file.OpenOption
   java.nio.file.Path
   java.nio.file.Paths
   java.nio.file.StandardCopyOption
   java.nio.file.StandardOpenOption
   java.nio.file.attribute.BasicFileAttributes
   java.nio.file.attribute.FileAttribute
   java.nio.file.attribute.FileTime
   java.util.EnumSet
   ))


(set! *warn-on-reflection* true)


;; * protocol


(defprotocol IPath
  (^java.nio.file.Path as-path [x]))


(defprotocol FilenameUtils
  (^String filename [this])
  (^String filepath [this])
  (^String parent [this])
  (^java.io.File parent-file [this])
  (^String basename [this])
  (^String extension [this]))


(defprotocol UriUtils
  (^java.net.URI to-uri [x]))


(defprotocol UrlUtils
  (^String to-external-form [url]))


(def default-url-utils-interface-impl
  {:to-external-form (fn [x] (to-external-form (jio/as-url x)))})


;; * nio.files


;;


(defn path ^Path [x] (as-path x))


;; ** resolution


(defn path-resolve
  ^Path
  [^Path a b]
  (. a resolve (str b)))


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


(defn ^"[Ljava.nio.file.OpenOption;"
  interpret-open-opts
  [opts]
  (into-array OpenOption (map interpret-open-opt opts)))


(defn ^"[Ljava.nio.file.CopyOption;"
  interpret-copy-opts
  [opts]
  (into-array CopyOption (map interpret-copy-opt opts)))


;; ** predicates


(defn exists?
  ^Boolean
  [x]
  (Files/exists (as-path x) *no-follow*))


(defn file?
  ^Boolean
  [x]
  (Files/isRegularFile (as-path x) *no-follow*))


(defn directory?
  ^Boolean
  [x]
  (Files/isDirectory (as-path x) *no-follow*))


(defn executable?
  ^Boolean
  [x]
  (Files/isExecutable (as-path x)))


(defn hidden?
  ^Boolean
  [x]
  (Files/isHidden (as-path x)))


(defn readable?
  ^Boolean
  [x]
  (Files/isReadable (as-path x)))


(defn writable?
  ^Boolean
  [x]
  (Files/isWritable (as-path x)))


(defn symlink?
  ^Boolean
  [x]
  (Files/isSymbolicLink (as-path x)))


(defn same-directory?
  ^Boolean
  [path1 path2]
  (let [normalized-path1 (.. (as-path path1) toAbsolutePath normalize)
        normalized-path2 (.. (as-path path2) toAbsolutePath normalize)]
    (.equals normalized-path2 normalized-path1)))


(defn parent-path?
  "Return true if `path1` is an ancestor path of `path2`"
  ^Boolean
  [path1 path2]
  (let [normalized-path1 (.. (as-path path1) toAbsolutePath normalize)
        normalized-path2 (.. (as-path path2) toAbsolutePath normalize)]
    (and
      (not (.equals normalized-path2 normalized-path1))
      (.startsWith normalized-path2 normalized-path1))))


;; ** operations


(defn mkdir
  ^Path
  [dirpath]
  (Files/createDirectories (as-path dirpath) (make-array FileAttribute 0)))


(defn mkparents
  ^Path
  [target]
  (when-let [parent (.getParent (as-path target))]
    (mkdir parent)))


(defn mktempdir
  ^Path
  ([^String prefix]
   (Files/createTempDirectory prefix (make-array FileAttribute 0)))
  ([^String dir ^String prefix]
   (Files/createTempDirectory (mkdir (as-path dir)) prefix (make-array FileAttribute 0))))


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
     (Files/move (as-path src) (as-path dst) opts))))


(defn copy!
  "
  - [src, dst] = [InputStream, Path]
  - [src, dst] = [Path, OutputStream]
  - [src, dst] = [Path, Path]
  "
  ([src dst]
   (cond
     (instance? OutputStream dst) (Files/copy src dst)
     :else                        (copy! src dst nil)))
  ([src dst attrs]
   (copy! src dst attrs #{:replace}))
  ([src dst {:keys [^FileTime time ^java.util.Set mode]} flags]
   (let [dst (as-path dst)]
     (cond
       (instance? InputStream src)
       (Files/copy ^InputStream src dst (interpret-copy-opts flags) )

       :else
       (Files/copy (as-path src) dst (interpret-copy-opts flags)))
     (when time (Files/setLastModifiedTime dst time))
     (when mode (Files/setPosixFilePermissions dst mode)))))


(defn write!
  ([target write-fn]
   (write! target write-fn #{:create}))
  ([target write-fn flags]
   (let [target (doto (as-path target) (mkparents))
         opts   (interpret-open-opts flags)]
     (with-open [os (Files/newOutputStream target opts)]
       (write-fn os)))))


;; ** batch operation


(defn- input-stream-or-as-path
  [x]
  (if (instance? InputStream x)
    x
    (as-path x)))


(defn- do-copy-operation
  [^Path dest-path {:keys [src path] :as operation}]
  (copy!
    (input-stream-or-as-path src)
    (doto (path-resolve dest-path path) (mkparents))
    (select-keys operation [:time :mode])))


(defn- do-write-operation
  [^Path dest-path {:keys [path write-fn] :as _operation}]
  (write! (doto (path-resolve dest-path path) (mkparents)) write-fn))


(defn do-operations
  [^Path dest-path operations]
  (run!
    (fn
      [operation]
      (try
        (case (:op operation)
          (:copy :copy!)   (do-copy-operation dest-path operation)
          (:write :write!) (do-write-operation dest-path operation)
          (throw (UnsupportedOperationException. (pr-str operation))))
        (catch Throwable e
          (throw (ex-info "Operation failed:" {:operation operation :exception e})))))
    operations))


;; * file tree


;;


(defn dirwalk-1
  "return lazy sequence"
  [dirpath re]
  (->> dirpath
    jio/file .listFiles seq
    (filter #(re-matches re ((memfn ^File getName) %)))))


(defn dirwalk
  "return lazy sequence"
  [dirpath re]
  (->> dirpath
    jio/file file-seq
    (filter #(re-matches re ((memfn ^File getName) %)))))


;; ** file visitor


(defrecord visitFileType [^Path path ^BasicFileAttributes attrs]
  clojure.lang.Indexed
  (nth [_this i] (case i 0 path 1 attrs (throw (IndexOutOfBoundsException.))))
  (nth [_this i not-found] (case i 0 path 1 attrs not-found))
  )


(defn make-file-visitor
  [visit-file]
  (reify FileVisitor
    (postVisitDirectory [_ _dir exception] (when exception (.printStackTrace exception)) FileVisitResult/CONTINUE)
    (preVisitDirectory [_ _dir _attrs] FileVisitResult/CONTINUE)
    (visitFile [_ path attrs]
      (visit-file (->visitFileType path attrs))
      FileVisitResult/CONTINUE)
    (visitFileFailed [_ _file exception]
      (case (.getName ^Class exception)
        "java.nio.file.FileSystemLoopException" FileVisitResult/SKIP_SUBTREE
        "java.nio.file.NoSuchFileException"     FileVisitResult/SKIP_SUBTREE
        (throw exception)))))


(defn transduce-file-tree-1
  ([xform rf path]
   (transduce-file-tree-1 xform rf (rf) path))
  ([xform rf *pointer path]
   (let [f (xform rf)]
     (Files/walkFileTree
       (as-path path)
       (EnumSet/of FileVisitOption/FOLLOW_LINKS)
       Integer/MAX_VALUE
       (make-file-visitor #(f *pointer %)))
     (rf *pointer))))


(defn transduce-file-tree
  ([xform rf path]
   (transduce-file-tree xform rf (rf) path))
  ([xform rf init path]
   (transduce-file-tree-1
     xform
     (fn
       ([tcoll] (rf (persistent! tcoll)))
       ([tcoll x] (rf tcoll x)))
     (transient init)
     path)))


;; ** fundamental operations for file-tree


(defn paths-copy-operations
  ([paths]
   (paths-copy-operations identity paths))
  ([wrap-xform paths]
   (let [tcoll (transient [])]
     (run!
       (fn [path]
         (when (directory? path)
           (transduce-file-tree-1
             (wrap-xform
               (map
                 (fn [[^Path path' ^BasicFileAttributes attrs]]
                   {:op   :copy
                    :src  path'
                    :path (.relativize (as-path path) path')
                    :time (. attrs lastModifiedTime)})))
             (fn
               ([_])
               ([tcoll op] (conj! tcoll op)))
             tcoll
             path)))
       paths)
     (persistent! tcoll))))


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


(defn file-basename
  ^String
  [^File file]
  (get-basename (. file getName)))


(defn file-extension
  ^String
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
  (as-path [x] x)

  jio/Coercions
  (as-file [x] (. x toFile))
  (as-url [x] (.. x toUri toURL))

  jio/IOFactory
  (make-input-stream [x opts] (jio/make-input-stream (Files/newInputStream x (make-array OpenOption 0)) opts))
  (make-output-stream [x opts] (jio/make-output-stream (Files/newOutputStream x (make-array OpenOption 0)) opts))
  (make-reader [x opts]
    (jio/make-reader
      (if-let [encoding (:encoding opts)]
        (Files/newBufferedReader x (Charset/forName encoding))
        (Files/newBufferedReader x))
      opts))
  (make-writer [x opts]
    (jio/make-writer
      (if-let [encoding (:encoding opts)]
        (Files/newBufferedWriter x (Charset/forName encoding) (make-array OpenOption 0))
        (Files/newBufferedWriter x (make-array OpenOption 0)))
      opts)))


(extend-type FileSystem
  IPath
  (as-path [x] (first (.getRootDirectories x))))


(extend-type String
  IPath
  (as-path [x] (Paths/get x (make-array String 0)))

  FilenameUtils
  (filename [this] (.getName (jio/file this)))
  (filepath [this] (.getPath (jio/file this)))
  (parent [this] (.getParent (jio/file this)))
  (parent-file [this] (.getParentFile (jio/file this)))
  (basename [this] (get-basename this))
  (extension [this] (get-extension this)))


(extend java.io.File
  IPath
  {:as-path (fn [^File x] (.toPath x))}

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
  {:to-external-form (fn [^File x] (.. x toURI toURL toExternalForm))})


(extend-type java.net.URI
  IPath
  (as-path [x] (Paths/get x))

  UriUtils
  (to-uri [x] x)

  UrlUtils
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
  (to-external-form [x] (. x toExternalForm)))


(set! *warn-on-reflection* false)
