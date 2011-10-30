;; -*- indent-tabs-mode: nil -*-

;;; seq_utils.clj -- Sequence utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; last updated March 2, 2009

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; Change Log
;;
;; January 10, 2009 (Stuart Sierra):
;;
;; * BREAKING CHANGE: "includes?" now takes collection as first
;;   argument.  This is more consistent with Clojure collection
;;   functions; see discussion at http://groups.google.com/group/clojure/browse_thread/thread/8b2c8dc96b39ddd7/a8866d34b601ff43



(ns midje.util.old-clojure-contrib.seq)

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

