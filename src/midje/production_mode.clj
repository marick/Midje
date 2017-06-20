(ns midje.production-mode)

;; This function is needed because midje.sweet is not fully
;; loaded when this file is loaded.
(letfn [(value-within [namespace-symbol variable-symbol]
          (if-let [namespace (find-ns namespace-symbol)]
            (var-get ((ns-map namespace) variable-symbol))
            (throw (Error. (str "It should be impossible for production mode to be checked before "
                                namespace-symbol
                                " is loaded.")))))]

  (defn user-desires-checking?
    "If clojure.test/*load-tests* or midje.sweet/include-midje-checks
    is false, facts won't run."
    []
    (and (value-within 'clojure.test '*load-tests*)
         (value-within 'midje.sweet 'include-midje-checks))))


