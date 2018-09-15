(ns jp.nijohando.fs
  (:refer-clojure :exclude [resolve])
  (:require [clojure.java.io :as jio])
  (:import (clojure.java.io Coercions
                            IOFactory)
           (java.io File
                    FileInputStream
                    FileOutputStream)
           (java.nio.file Files
                          FileVisitor
                          FileVisitOption
                          FileVisitResult
                          LinkOption
                          Path
                          Paths)
           (java.nio.file.attribute FileAttribute
                                    FileTime)))

(def ^:dynamic *link-options* (into-array LinkOption []))
(def ^:dynamic *cwd* (.. (File. ".") toPath (toRealPath *link-options*)))

(def ^:private empty-file-attributes (into-array FileAttribute []))

(defn- file-attrs
  [attributes]
  (->> (or attributes empty-file-attributes)
       (into-array FileAttribute)))

(extend Path
  jio/Coercions
  {:as-file (fn [this]
              (.toFile this))
   :as-url (fn [this]
             (.. this (toUri) (toURL)))}

  jio/IOFactory
  (assoc jio/default-streams-impl
         :make-input-stream (fn [^Path x opts] (jio/make-input-stream (. x toFile) opts))
         :make-output-stream (fn [^Path x opts] (jio/make-output-stream (. x toFile) opts))))

(defn path?
  [x]
  (instance? Path x))

(defmulti to-path class)
(defmethod to-path Path [p]
  p)
(defmethod to-path String [s]
  (to-path (Paths/get s (into-array String []))))
(defmethod to-path File [f]
  (to-path (.toPath f)))
(defmethod to-path nil [_]
  nil)

(defmacro with-nofllow-links
  [& forms]
  `(binding [*link-options* (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])]
    ~@forms))

(defmacro with-cwd
  [cwd & forms]
  `(binding [*cwd* (path ~cwd)]
     ~@forms))

(defn path
  ([x]
   (to-path x))
  ([parent & children]
   (loop [p (to-path parent)
          [s & more] children]
     (if (or (nil? p) (nil? (not-empty s)))
       p
       (recur (.resolve p s) more)))))

(defn file
  [& xs]
  (some-> (apply path xs)
          (.toFile)))

(defn path-seq
  [x]
  (->> (file x)
       file-seq
       (map path)))

(defn real-path
  [x]
  (-> (path x)
      (.toRealPath *link-options*)))

(defn symbolic-link?
  [x]
  (when-some [p (path x)]
    (Files/isSymbolicLink p)))

(defn dir?
  [x]
  (when-some [p (path x)]
    (Files/isDirectory p *link-options*)))

(defn file?
  [x]
  (when-some [p (path x)]
    (Files/isRegularFile p *link-options*)))


(defn exists?
  [x]
  (-> (path x)
      (Files/exists *link-options*)))

(defn filename
  [x]
  (.getFileName (path x)))

(defn parent
  [x]
  (.getParent (path x)))

(defn delete-dir
  [x]
  (letfn [(del [file]
            (when (.isDirectory file)
              (doseq [child-file (.listFiles file)]
                (del child-file)))
            (clojure.java.io/delete-file file))]
    (let [f (.toFile (path x))]
      (when (.exists f)
        (del f)))))

(defn create-dir
  ([x]
   (create-dir x nil))
  ([x file-attributes]
   (-> (path x)
       (Files/createDirectories (file-attrs file-attributes)))))

(defn create-temp-dir
  ([prefix]
   (create-temp-dir prefix nil))
  ([prefix file-attributes]
   (Files/createTempDirectory prefix (file-attrs file-attributes))))

(defn create-file
  ([x]
   (create-file x nil))
  ([x file-attributes]
   (-> (path x)
       (Files/createFile (file-attrs file-attributes)))))

(defn create-symbolic-link
  ([link target]
   (create-symbolic-link link target nil))
  ([link target file-attributes]
   (Files/createSymbolicLink (path link) (path target) (file-attrs file-attributes))))

(defn relativize
  [base other]
  (.relativize (path base) (path other)))

(defn absolute
  [x]
  (-> (path x)
      (.toAbsolutePath)))

(defn resolve
  [base other]
  (.resolve (path base) (path other)))

