(ns ^{:doc "Environmental factors."}
  midje.util.ecosystem
  (:require [clojure.string :as str]
            [leiningen.core.project :as project]))

(def issues-url "https://github.com/marick/Midje/issues")

(defn clojure-1-3? []
  (and (= 1 (:major *clojure-version*))
       (= 3 (:minor *clojure-version*))))

(defn clojure-1-2-X? []
  (and (= 1 (:major *clojure-version*))
       (= 2 (:minor *clojure-version*))))

(defn clojure-1-2-0? []
  (and (= 1 (:major *clojure-version*))
       (= 2 (:minor *clojure-version*))
       (= 0 (:incremental *clojure-version*))))

(defmacro when-1-3+ [& body] 
  (when-not (= 2 (:minor *clojure-version*))
    `(do ~@body)))

(defmacro unless-1-2-0
  "Skip body completely - including 'Unable to resolve classname' errors."
  [& body]
  (when-not (clojure-1-2-0?)
    `(do ~@body)))

;; The following works because in 1.2 it's parsed as [+  '1].

(def +M (first [+' 1]))
(def -M (first [-' 1]))
(def *M (first [*' 1]))

;;

(defn getenv [var] 
  (System/getenv var))

(defn on-windows? []
  (re-find #"[Ww]in" (System/getProperty "os.name")))

(def line-separator (System/getProperty "line.separator"))

(def home-config-file-name (str/join java.io.File/separator
                                     [(getenv "HOME") ".midje.clj"]))
(def project-config-file-name ".midje.clj")

(defn- file-exists? [name]
  (.isFile (new java.io.File name)))

(defn has-home-config-file? []
  (and (getenv "HOME") (file-exists? home-config-file-name)))

(defn has-project-config-file? []
  (file-exists? project-config-file-name))

(def config-files
  (keep identity
        [(if (has-home-config-file?)    home-config-file-name)
         (if (has-project-config-file?) project-config-file-name)]))

(defn set-config-files! [files]
  (alter-var-root #'config-files (constantly files)))


;; This is kludgy. We can get the Leiningen profile information
;; from `lein-midje` if that task sets this atom. There's no way to
;; get that from `lein repl`, so the default value from `project.clj`
;; is returned.

(when-1-3+

 (def leiningen-paths-var nil)

 (defmacro around-initial-paths [& body]
   `(let [original# leiningen-paths-var]
      (try
        (alter-var-root #'leiningen-paths-var (constantly nil))
        ~@body
        (finally (alter-var-root #'leiningen-paths-var (constantly original#))))))
 
 (defn set-leiningen-paths! [project]
   ;; Note that the order is guaranteed: test paths come before project paths.
   (alter-var-root #'leiningen-paths-var
                   (constantly (concat (:test-paths project) (:source-paths project)))))

 (defn- project-with-paths []
   (try
     (project/read)
   (catch java.io.FileNotFoundException e
     {:test-paths ["test"]})))
 
 (defn leiningen-paths []
   (or leiningen-paths-var
       (do
         (set-leiningen-paths! (project-with-paths))
         leiningen-paths-var)))
       
)
