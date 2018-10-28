(ns jp.nijohando.fs-test
  (:refer-clojure :exclude [resolve])
  (:require [clojure.test :as t :refer [run-tests is are deftest testing use-fixtures]]
            [clojure.java.io :as jio]
            [jp.nijohando.fs :as fs :refer [*cwd*]])
  (:import (java.io File)
           (java.nio.file Path
                          Files)))

(defn- create-test-dir
  []
  (fs/create-temp-dir "jp.nijohando.fs"))

(defn- delete-test-dir
  []
  (fs/delete-dir *cwd*))

(defn- with-test-dir
  [f]
  (fs/with-cwd (create-test-dir)
    (f)
    (delete-test-dir)))

(use-fixtures :each with-test-dir)

(deftest path
  (testing "Path creation"
    (testing "Path can be created from a string"
      (let [p (fs/path "/tmp")]
        (is (instance? Path p))
        (is (= "/tmp" (str p)))))
    (testing "Path can be created from strings"
      (let [p (fs/path "/tmp" "foo" "bar")]
        (is (instance? Path p))
        (is (= "/tmp/foo/bar" (str p)))))
    (testing "Path can be created from a java.io.File"
      (let [f (File. "/tmp")
            p (fs/path f)]
        (is (instance? Path p))
        (is (= "/tmp" (str p)))))
    (testing "Path can be created from a java.io.File and strings"
      (let [f (File. "/tmp")
            p (fs/path f "foo" "bar")]
        (is (instance? Path p))
        (is (= "/tmp/foo/bar" (str p)))))
    (testing "Path is passed through without conversion"
      (let [p1 (fs/path "/tmp")
            p2 (fs/path p1)]
        (is (instance? Path p2))
        (is (= "/tmp" (str p2)))
        (is (= p1 p2))))
    (testing "Path can be created from java.nio.file.Path and strings"
      (let [p1 (fs/path "/tmp")
            p2 (fs/path p1 "foo" "bar")]
        (is (instance? Path p2))
        (is (= "/tmp/foo/bar" (str p2)))))
    (testing "Nil is passed through wihtout any error"
      (let [p (fs/path nil)]
        (is (nil? p))))
    (testing "Nil with strings is passed through wihtout any error"
      (let [p (fs/path nil "foo" "bar")]
        (is (nil? p)))))
  (testing "Path assertion"
    (is (fs/path? (fs/path "/tmp")))))

(deftest file
  (testing "File creation"
    (testing "File can be created from a string"
      (let [f (fs/file "/tmp")]
        (is (instance? File f))
        (is (= "/tmp" (str f)))))
    (testing "File can be created from strings"
      (let [f (fs/file "/tmp" "foo" "bar")]
        (is (instance? File f))
        (is (= "/tmp/foo/bar" (str f)))))
    (testing "File can be created from a java.nio.file.Path"
      (let [p (fs/path "/tmp")
            f (fs/file p)]
        (is (instance? File f))
        (is (= "/tmp" (str f)))))
    (testing "File can be created from a java.nio.file.Path and strings"
      (let [p (fs/path "/tmp" "foo" "bar")
            f (fs/file p)]
        (is (instance? File f))
        (is (= "/tmp/foo/bar" (str f)))))
    (testing "File is passed through without conversion"
      (let [f1 (fs/file "/tmp")
            f2 (fs/file f1)]
        (is (instance? File f2))
        (is (= "/tmp" (str f2)))
        (is (= f1 f2))))
    (testing "File can be created from java.io.File and strings"
      (let [f1 (fs/file "/tmp")
            f2 (fs/file f1 "foo" "bar")]
        (is (instance? File f2))
        (is (= "/tmp/foo/bar" (str f2)))))
    (testing "Nil is passed through wihtout any error"
      (let [f (fs/file nil)]
        (is (nil? f))))
    (testing "Nil with strings is passed through wihtout any error"
      (let [f (fs/file nil "foo" "bar")]
        (is (nil? f))))))

(deftest path-seq
  (testing "Paths can be get under the dir"
    (spit (fs/path *cwd* "foo.txt") "foo")
    (spit (fs/path *cwd* "bar.txt") "bar")
    (let [xs (fs/path-seq *cwd*)]
      (is (= 3 (count xs)))
      (doseq [x xs]
        (is (instance? Path x)))
      (is (= (fs/path *cwd*) (first xs)))
      (let [files (->> (rest xs)
                       (sort))]
        (is (= (fs/path *cwd* "bar.txt") (first files)))
        (is (= (fs/path *cwd* "foo.txt") (second files)))))))

(deftest symbolic-link?
  (testing "Symbolic link must be true"
    (let [link (->> (fs/create-dir (fs/path *cwd* "dir1"))
                    (fs/create-symbolic-link (fs/path *cwd* "dir1-link")))]
      (is (fs/symbolic-link? link))))
  (testing "Existing dir must be false"
    (is (not (fs/symbolic-link? (fs/path *cwd*)))))
  (testing "Nonexisting file must be false"
    (is (not (fs/symbolic-link? (fs/path *cwd* "nil.txt"))))))

(deftest dir?
  (testing "Existing dir must be true"
    (is (fs/dir? (fs/path *cwd*))))
  (testing "Existing file must be false"
    (let [p (fs/path *cwd* "file1.txt")]
      (spit p "foo")
      (is (not (fs/dir? p)))))
  (testing "Nonexisting file must be false"
    (is (not (fs/dir? (fs/path *cwd* "nil.txt")))))
  (testing "Symbolic link to existing dir must be true (follow)"
    (let [link (->> (fs/create-dir (fs/path *cwd* "dir1"))
                    (fs/create-symbolic-link (fs/path *cwd* "dir1-link")))]
      (is (fs/dir? link))))
  (testing "Symbolic link to existing dir must be false (nofollow)"
    (let [link (->> (fs/create-dir (fs/path *cwd* "dir2"))
                    (fs/create-symbolic-link (fs/path *cwd* "dir2-link")))]
      (fs/with-nofllow-links
        (is (not (fs/dir? link)))))))

(deftest file?
  (testing "Existing file must be true"
    (let [p (fs/path *cwd* "file1.txt")]
      (spit p "foo")
      (is (fs/file? p))))
  (testing "Existing dir must be false"
    (is (not (fs/file? (fs/path *cwd*)))))
  (testing "Nonexisting file must be false"
    (is (not (fs/file? (fs/path *cwd* "nil.txt")))))
  (testing "Symbolic link to existing file must be true (follow)"
    (let [link (->> (fs/create-file (fs/path *cwd* "file2.txt"))
                    (fs/create-symbolic-link (fs/path *cwd* "file2-link")))]
      (is (fs/file? link))))
  (testing "Symbolic link to existing file must be false (nofollow)"
    (let [link (->> (fs/create-file (fs/path *cwd* "file3.txt"))
                    (fs/create-symbolic-link (fs/path *cwd* "file3-link")))]
      (fs/with-nofllow-links
        (is (not (fs/file? link)))))))

(deftest exists?
  (testing "Existing dir must be true"
    (is (fs/exists? (fs/path *cwd*))))
  (testing "Existing file must be true"
    (let [p (fs/path *cwd* "file1.txt")]
      (spit p "foo")
      (is (fs/exists? p))))
  (testing "Nonexisting file must be false"
    (is (not (fs/file? (fs/path *cwd* "nil.txt")))))
  (testing "Symbolic link to existing file must be true (follow)"
    (let [link (->> (fs/create-file (fs/path *cwd* "file2.txt"))
                    (fs/create-symbolic-link (fs/path *cwd* "file2-link")))]
      (is (fs/file? link))))
  (testing "Symbolic link to existing file must be false (nofollow)"
    (let [link (->> (fs/create-file (fs/path *cwd* "file3.txt"))
                    (fs/create-symbolic-link (fs/path *cwd* "file3-link")))]
      (fs/with-nofllow-links
        (is (not (fs/file? link)))))))

(deftest filename
  (testing "Filename can be obtained from string"
    (is (= (fs/path "bar.txt" ) (fs/filename "/tmp/foo/bar.txt"))))
  (testing "Filename can be obtained from java.nio.file.Path"
    (is (= (fs/path "bar.txt" ) (fs/filename (fs/path "/tmp/foo/bar.txt" )))))
  (testing "Filename can be obtained from java.io.File"
    (is (= (fs/path "bar.txt" ) (fs/filename (fs/file "/tmp/foo/bar.txt" ))))))

(deftest parent
  (testing "Parent path can be obtained from string"
    (is (= (fs/path "/tmp/foo" ) (fs/parent "/tmp/foo/bar.txt"))))
  (testing "Parent path can be obtained from java.nio.file.Path"
    (is (= (fs/path "/tmp/foo" ) (fs/parent (fs/path "/tmp/foo/bar.txt" )))))
  (testing "Parent path can be obtained from java.io.File"
    (is (= (fs/path "/tmp/foo" ) (fs/parent (fs/file "/tmp/foo/bar.txt" ))))))

(deftest delete-dir
  (testing "Directory can be deleted"
    (let [d (fs/create-dir (fs/path *cwd* "dir1"))]
      (is (fs/exists? d))
      (fs/delete-dir d)
      (is (not (fs/exists? d)))))
  (testing "Directory that contains children can be deleted"
    (let [d (fs/create-dir (fs/path *cwd* "dir2"))
          f (fs/create-file (fs/path d "file2-1.txt"))]
      (is (fs/exists? d))
      (is (fs/exists? f))
      (fs/delete-dir d)
      (is (not (fs/exists? d)))))
  (testing "File can be also deleted"
    (let [f (fs/create-file (fs/path *cwd* "file1"))]
      (is (fs/exists? f))
      (fs/delete-dir f)
      (is (not (fs/exists? f)))))
  (testing "Nonexisting path can be done without any error"
    (let [p (fs/path *cwd* "nil")]
      (is (not (fs/exists? p)))
      (fs/delete-dir p))))

(deftest create-dir
  (testing "Directory can be created under existing parent directory"
    (let [d (fs/create-dir (fs/path *cwd* "dir1"))]
      (is (fs/exists? d))
      (is (fs/dir? d))))
  (testing "Directory can be created under noexistent parent directory"
    (let [d (fs/create-dir (fs/path *cwd* "dir2" "dir2-1"))]
      (is (fs/exists? d))
      (is (fs/dir? d))))
  (testing "No error should occur event if the directory already exists"
    (let [d (fs/create-dir (fs/path *cwd* "dir3"))]
      (is (fs/exists? d))
      (is (fs/dir? d)))
    (let [d (fs/create-dir (fs/path *cwd* "dir3"))]
      (is (fs/exists? d))
      (is (fs/dir? d)))))

(deftest create-file
  (testing "File can be created under existing parent diretory"
    (let [f (fs/create-file (fs/path *cwd* "file1"))]
      (is (fs/exists? f))
      (is (fs/file? f))))
  (testing "File can't be created under noexistent parent directory"
    (is (thrown? java.nio.file.NoSuchFileException (fs/create-file (fs/path *cwd* "dir2" "file2-1")))))
  (testing "File can't be created if the file already exists"
    (let [f (fs/path *cwd* "file3")]
      (fs/create-file f)
      (fs/exists? f)
      (is (thrown? java.nio.file.FileAlreadyExistsException (fs/create-file f))))))

(deftest create-symbolic-link
  (testing "Symbolic link can be created under existing parent diretory"
    (let [f (fs/create-file (fs/path *cwd* "file1"))
          l (fs/create-symbolic-link (fs/path *cwd* "link-file1") f)]
      (is (fs/symbolic-link? l))))
  (testing "Symbolic link can't be created under nonexistent parent diretory"
    (let [f (fs/create-file (fs/path *cwd* "filer2"))]
      (is (thrown? java.nio.file.NoSuchFileException (fs/create-symbolic-link (fs/path *cwd* "dir1" "link-file1-1") f)))))
  (testing "Symbolic link can't be created if the file already exists"
    (let [f3 (fs/create-file (fs/path *cwd* "file3"))
          f4 (fs/create-file (fs/path *cwd* "file4"))]
      (is (thrown? java.nio.file.FileAlreadyExistsException (fs/create-symbolic-link f4 f3))))))

(deftest relativize
  (testing "String path can be relativized"
    (is (= (fs/path "bar.txt") (fs/relativize "/tmp/foo" "/tmp/foo/bar.txt")))
    (is (= (fs/path "bar.txt") (fs/relativize "/tmp/foo" (fs/path "/tmp/foo/bar.txt"))))
    (is (= (fs/path "bar.txt") (fs/relativize "/tmp/foo" (fs/file "/tmp/foo/bar.txt")))))
  (testing "Parh can be relativized"
    (is (= (fs/path "bar.txt") (fs/relativize (fs/path "/tmp/foo") "/tmp/foo/bar.txt")))
    (is (= (fs/path "bar.txt") (fs/relativize (fs/path "/tmp/foo") (fs/path "/tmp/foo/bar.txt"))))
    (is (= (fs/path "bar.txt") (fs/relativize (fs/path "/tmp/foo") (fs/file "/tmp/foo/bar.txt")))))
  (testing "File can be relativized"
    (is (= (fs/path "bar.txt") (fs/relativize (fs/file "/tmp/foo") "/tmp/foo/bar.txt")))
    (is (= (fs/path "bar.txt") (fs/relativize (fs/file "/tmp/foo") (fs/path "/tmp/foo/bar.txt"))))
    (is (= (fs/path "bar.txt") (fs/relativize (fs/file "/tmp/foo") (fs/file "/tmp/foo/bar.txt"))))))

(deftest absolute
  (testing "Absolute path can be obtained"
    (let [d (.. (File. ".") toPath (toRealPath fs/*link-options*))]
      (is (= (fs/path d "foo.txt") (fs/absolute "foo.txt"))))))

(deftest resolve
  (testing "String path can be resolved"
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve "/tmp" "foo/bar.txt")))
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve "/tmp" (fs/path "foo/bar.txt" ))))
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve "/tmp" (fs/file "foo/bar.txt" )))))
  (testing "Path path can be resolved"
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve (fs/path "/tmp") "foo/bar.txt")))
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve (fs/path "/tmp") (fs/path "foo/bar.txt"))))
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve (fs/path "/tmp") (fs/file "foo/bar.txt")))))
  (testing "File path can be resolved"
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve (fs/file "/tmp") "foo/bar.txt")))
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve (fs/file "/tmp") (fs/path "foo/bar.txt"))))
    (is (= (fs/path "/tmp/foo/bar.txt") (fs/resolve (fs/file "/tmp") (fs/file "foo/bar.txt"))))))
