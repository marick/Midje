(ns ^{:doc "Checkers that combine other checkers."}
  midje.checking.checkers.combining
  (:use midje.checking.core
        midje.checking.checkers.defining
        midje.checking.checkers.chatty))

(defn report-failure [actual checker-form result]
  (as-data-laden-falsehood {:actual actual
                            :intermediate-results
                            [ [checker-form (user-friendly-falsehood result)]]}))

(defn- build-check-form [checker-form actual-sym]
  `(if (instance? java.util.regex.Pattern ~checker-form)
     (re-matches ~checker-form ~actual-sym)
     (~checker-form ~actual-sym)))

(defn- ^{:testable true}
  wrap-in-and-checking-form [checker-form to-wrap result-sym actual-sym]
  (let [check-form (build-check-form checker-form actual-sym)]
    `(let [~result-sym ~check-form]
    (if (extended-false? ~result-sym)
      (report-failure ~actual-sym '~checker-form ~result-sym)
      ~to-wrap))))

(defmacro every-checker
  "Combines multiple checkers into one checker that passes 
   when all component checkers pass. If one checker fails,
   the remainder are not run. The output shows which checker
   failed.

   Example:

       (fact 3 => (every-checker odd? neg?))
       FAIL ...
       ...
       During checking, these intermediate values were seen:
          neg? => false
  "
  [& checker-forms]
  (let [actual-gensym (gensym "actual-result-")
        check-result-gensym (gensym "check-result-")
        checks-form (reduce (fn [to-wrap checker-form] 
                              (wrap-in-and-checking-form checker-form to-wrap
                                                         check-result-gensym
                                                         actual-gensym))
                            'true
                            (reverse checker-forms))]
    `(checker [~actual-gensym] ~checks-form)))
                       

(defn- ^{:testable true}
  wrap-in-or-checking-form [checker-form to-wrap actual-sym]
  (let [check-form (build-check-form checker-form actual-sym)]
    `(if (extended-true? ~check-form)
       true
       ~to-wrap)))

(defmacro some-checker
  "Combines multiple checkers into one checker that passes 
   when any of the component checkers pass. If one checker
   passes, the remainder are not run. Example:

      (fact 3 => (some-checker even? neg?)) ; fails
   "
  [& checker-forms]
  (let [actual-gensym (gensym "actual-result-")
        checks-form (reduce (fn [to-wrap checker-form] 
                              (wrap-in-or-checking-form checker-form to-wrap
                                                        actual-gensym))
                            'false
                            (reverse checker-forms))]
    `(checker [~actual-gensym] ~checks-form)))
