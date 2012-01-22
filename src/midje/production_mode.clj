;; -*- indent-tabs-mode: nil -*-

(ns midje.production-mode)

(letfn [(value-within [namespace-symbol variable-symbol]
          (if-let [namespace (find-ns namespace-symbol)]
            (var-get ((ns-map namespace) variable-symbol))
            true))]

  (defn user-desires-checking? []
    (and (value-within 'clojure.test '*load-tests*)
         (value-within 'midje.sweet '*include-midje-checks*)
         (value-within 'midje.semi-sweet '*include-midje-checks*))))


