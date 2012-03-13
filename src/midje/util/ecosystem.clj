;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Environmental factors."}
  midje.util.ecosystem)

(def issues-url "https://github.com/marick/Midje/issues")

(defn clojure-1-3? []
  (and (= 1 (:major *clojure-version*))
       (= 3 (:minor *clojure-version*))))

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
