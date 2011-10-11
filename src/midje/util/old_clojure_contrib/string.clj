;;; str_utils3.clj -- functional string utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; January 26, 2010

;; Copyright (c) Stuart Sierra, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns #^{:author "Stuart Sierra"
       :doc "This is a library of string manipulation functions.  It
    is intented as a replacement for clojure.contrib.string.

    You cannot (use 'clojure.contrib.string) because it defines
    functions with the same names as functions in clojure.core.
    Instead, do (require '[clojure.contrib.string :as s]) 
    or something similar.

    Goals:
      1. Be functional
      2. Most significant argument LAST, to work with ->>
      3. At least O(n) performance for Strings of length n

    Some ideas are borrowed from
    http://github.com/francoisdevlin/devlinsf-clojure-utils/"}
 midje.util.old-clojure-contrib.string
 (:refer-clojure :exclude (take replace drop butlast partition
                           contains? get repeat reverse partial))
 (:import (java.util.regex Pattern)))


(defn #^String butlast
  "Returns s without the last n characters.  Returns an empty string
  if n is greater than the length of s."
  [n #^String s]
  (if (< (count s) n)
    ""
    (.substring s 0 (- (count s) n))))

(defn #^String substring?
  "True if s contains the substring."
  [substring #^String s]
  (.contains s substring))
