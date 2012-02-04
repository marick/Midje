;; -*- indent-tabs-mode: nil -*-

(ns midje.cljs
  (:require [cljs.closure :as cljsc]
            [cljs.repl :as repl]
            [cljs.compiler :as comp]
            [cljs.repl.rhino :as rhino]))

(def ^:dynamic *env* (rhino/repl-env))

(defn load-file [f] (repl/load-file *env* f))

(defn- wrap-fn
  "borrowed from cljs.repl (declared private in that ns)"
  [form]
  (cond (and (seq? form) (= 'ns (first form))) identity
        ('#{*1 *2 *3} form) (fn [x] `(cljs.core.pr-str ~x))
        :else (fn [x] `(cljs.core.pr-str
                        (let [ret# ~x]
                          (do (set! *3 *2)
                              (set! *2 *1)
                              (set! *1 ret#)
                              ret#))))))

(defn cljs-eval
  "Usage:
    (cljs-eval '(+ 1 2))
    (cljs-eval '(doubler 4) 'midje.cljs.basic)"
  [form & [cljs-ns]]
  (let [env {:ns (@comp/namespaces comp/*cljs-ns*) :context :statement :locals {}}]
    (read-string (repl/evaluate-form *env*
                                     (if cljs-ns (assoc env :ns {:name cljs-ns}) env)
                                     "<cljs repl>"
                                     form
                                     (wrap-fn form)))))

(load-file "cljs/core.cljs")