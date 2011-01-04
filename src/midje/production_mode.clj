;; -*- indent-tabs-mode: nil -*-

(ns midje.production-mode)

(defn- value-within [namespace-symbol variable-symbol]
  (let [namespace (find-ns namespace-symbol)]
    (if namespace
      (var-get ((ns-map namespace) variable-symbol))
      true)))

(defn user-desires-checking? []
  (and (value-within 'clojure.test '*load-tests*)
       (value-within 'midje.sweet '*include-midje-checks*)
       (value-within 'midje.semi-sweet '*include-midje-checks*)))


