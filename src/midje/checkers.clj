(ns ^{:doc "Checkers are for checking results of checkables, or checking 
            that appropriate arguments are passed to prerequisites"} 
  midje.checkers
  (:use marick.clojure.core))


(when-not (resolve '&)
  (let [docstring "This var is defined so that Midje prerequisites can use & for optional args without having to quote it."]
    (intern *ns* (vary-meta '& assoc :doc docstring) docstring)))

(letfn [(republish [namespace symbols]
          (require namespace)
          (doseq [sym symbols]
            (let [^clojure.lang.Var var ((ns-publics namespace) sym)]
              (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
                (if (.hasRoot var)
                  (intern *ns* sym (var-root var))
                  (intern *ns* sym)))))) ]

  (republish 'midje.checking.checkers.defining
             '[defchecker checker as-checker])
  (republish 'midje.checking.checkers.chatty
             '[chatty-checker])
  (republish 'midje.checking.checkers.simple
             '[truthy falsey TRUTHY FALSEY anything irrelevant exactly throws roughly])
  (republish 'midje.checking.checkers.combining
             '[every-checker some-checker])
  (republish 'midje.checking.checkers.collection
             '[has has-suffix has-prefix just contains n-of
              one-of two-of three-of four-of five-of six-of seven-of eight-of nine-of ten-of]))
