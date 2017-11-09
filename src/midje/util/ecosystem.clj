(when (and (= 1 (:major *clojure-version*))
           (#{2 3} (:minor *clojure-version*)))
  (println "=====================================================")
  (println "==== Midje no longer supports Clojure 1.2 or 1.3 ====")
  (println "=====================================================")
  (println))

(ns ^{:doc "Environmental factors."}
  midje.util.ecosystem
  (:require [clojure.string :as str]
            such.versions)
  (:import [java.io File]))

(def issues-url "https://github.com/marick/Midje/issues")
(def syntax-errors-that-will-not-be-fixed
  "https://github.com/marick/Midje/wiki/Syntax-errors-that-will-not-be-fixed")


(defmacro when-1-6+ [& body]
  (when (>= (:minor *clojure-version*) 6)
    `(do ~@body)))

(defmacro when-1-7+ [& body]
  (when (>= (:minor *clojure-version*) 7)
    `(do ~@body)))

;;

(defn getenv [var]
  (System/getenv var))

(defn on-windows? []
  (boolean (re-find #"[Ww]in" (System/getProperty "os.name"))))

(def line-separator (System/getProperty "line.separator"))

(def home-config-file-name (str/join File/separator
                                     [(getenv "HOME") ".midje.clj"]))
(def project-config-file-name ".midje.clj")

(defn- file-exists? [^String name]
  (.isFile (File. name)))

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
(defmacro #^:private defproject [name version & {:as args}]
  `(set-leiningen-paths! (merge {:test-paths ["test"] :source-paths ["src"]} '~args)))

(defn- set-leiningen-paths-from-project-file! []
  (binding [*ns* (find-ns 'midje.util.ecosystem)]
    (try
      (load-file "project.clj")
    (catch java.io.FileNotFoundException e
        (set-leiningen-paths! {:test-paths ["test"]})))))

(defn leiningen-paths []
  (or leiningen-paths-var
      (do
        (set-leiningen-paths-from-project-file!)
        leiningen-paths-var)))
