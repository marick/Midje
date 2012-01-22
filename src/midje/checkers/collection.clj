;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns ^{:doc "Checkers for collections and strings."}
  midje.checkers.collection
  (:use [clojure.set :only [union]]
        [clojure.pprint :only [cl-format]]
        [midje.util.backwards-compatible-utils :only [every-pred-m]] 
        [midje.util.form-utils :only [regex? record? classic-map? pred-cond macro-for]]
      	[midje.checkers collection-util util extended-equality chatty defining collection-comparison]
        [midje.error-handling.exceptions :only [user-error]]))

(def ^:private looseness-modifiers #{:in-any-order :gaps-ok})

(defn- separate-looseness
  "Distinguish expected results from looseness descriptions.
   1 :in-any-order => [1 [:in-any-order]]
   1 2 :in-any-order => [ [1 2] [:in-any-order] ]
   Single elements require specialized processing, so they're left alone.
   More than one element must be an indication that a sequential is desired."
  [args]
  (letfn [(special-case-singletons [x] (if (= 1 (count x)) (first x) x))
          (past-end-of-real-arguments [x] (or (empty? x)
                                              (some looseness-modifiers [(first x)])))]
  (loop [known-to-be-expected [(first args)]
         might-be-looseness-modifier (rest args)]
    (if (past-end-of-real-arguments might-be-looseness-modifier)
      (vector (special-case-singletons known-to-be-expected) might-be-looseness-modifier)
      (recur (conj known-to-be-expected (first might-be-looseness-modifier))
             (rest might-be-looseness-modifier))))))

(letfn [(compatibility-check
          ;;Fling an error if the combination of actual, expected, and looseness won't work.
          [actual expected looseness]

          ;; Throwing Errors is just an implementation convenience.
          (cond (regex? expected)
            (cond (and (not (sequential? actual))
                       (not (empty? looseness)))
              (throw (user-error (str "I don't know how to make sense of a "
                                   "regular expression applied "
                                   looseness "."))))

            (not (collection-like? actual))
            (throw (user-error (str "You can't compare " (pr-str actual) " (" (type actual)
                                 ") to " (pr-str expected) " (" (type expected) ").")))

            (and (record? expected)
                 (map? actual)
                 (not (= (class expected) (class actual))))
            (throw (user-error (str "You expected a " (.getName (class expected))
                                 " but the actual value was a "
                                 (if (classic-map? actual) "map" (.getName (class actual)))
                                 ".")))

            (and (map? actual)
                 (not (map? expected)))
            (try (into {} expected)
              (catch Throwable ex
                (throw (user-error (str "If " (pr-str actual) " is a map, "
                                     (pr-str expected)
                                     " should look like map entries.")))))))
        (standardized-arguments
          ;;Reduce arguments to standard forms so there are fewer combinations to
          ;;consider. Also blow up for some incompatible forms."
          [actual expected looseness]
          (compatibility-check actual expected looseness)
          (pred-cond actual
            sequential?
            (pred-cond expected
              set? [actual (vec expected) (union looseness #{:in-any-order })]
              right-hand-singleton? [actual [expected] (union looseness #{:in-any-order })]
              :else [actual expected looseness])

            map?
            (pred-cond expected
              map? [actual expected looseness]
              :else [actual (into {} expected) looseness])

            set?
            (recur (vec actual) expected looseness-modifiers)

            string?
            (pred-cond expected
              (every-pred-m (complement string?) (complement regex?)) (recur (vec actual) expected looseness)
              :else [actual expected looseness])

            :else [actual expected looseness]))

        (match? [actual expected looseness]
          (let [comparison (compare-results actual expected looseness)]
            (or (total-match? comparison)
              (apply noted-falsehood
                (cons (best-actual-match (midje-classification actual) comparison)
                  (best-expected-match (midje-classification actual) comparison expected))))))

        (container-checker-maker [name checker-fn]
          (checker [& args]
            (let [[expected looseness] (separate-looseness args)]
              (as-chatty-checker
                (named-as-call name expected
                  (fn [actual]
                    (add-actual actual
                      (try (checker-fn actual expected looseness)
                        (catch Error ex
                          (noted-falsehood (.getMessage ex)))))))))))

        (has-xfix [x-name pattern-fn take-fn]
          (checker [actual expected looseness]
            (pred-cond actual
              set? (noted-falsehood (format "Sets don't have %ses." x-name))
              map? (noted-falsehood (format "Maps don't have %ses." x-name))
              :else (let [[actual expected looseness] (standardized-arguments actual expected looseness)]
                      (cond (regex? expected)
                        (try-re (pattern-fn expected) actual re-find)

                        (expected-fits? actual expected)
                        (match? (take-fn (count expected) actual) expected looseness)

                        :else (noted-falsehood
                                (cl-format nil
                                  "A collection with ~R element~:P cannot match a ~A of size ~R."
                                  (count actual) x-name (count expected))))))))]

  (def ^{:midje/checker true} has-prefix
    (container-checker-maker 'has-prefix
      (has-xfix "prefix" #(re-pattern (str "^" %)) take)))

  (def ^{:midje/checker true} has-suffix
    (container-checker-maker 'has-suffix
      (has-xfix "suffix" #(re-pattern (str % "$")) take-last)))


  (def ^{:midje/checker true} contains (container-checker-maker 'contains
                                         (fn [actual expected looseness]
                                           (let [ [actual expected looseness] (standardized-arguments actual expected looseness)]
                                             (cond (regex? expected)
                                               (try-re expected actual re-find)

                                               :else
                                               (match? actual expected looseness))))))

  (def ^{:midje/checker true} just (container-checker-maker 'just
                                     (fn [actual expected looseness]
                                       (let [ [actual expected looseness] (standardized-arguments actual expected looseness)]
                                         (cond (regex? expected)
                                           (try-re expected actual re-matches)

                                           (same-lengths? actual expected)
                                           (match? actual expected looseness)

                                           :else
                                           (noted-falsehood
                                             (cl-format nil "Expected ~R element~:P. There ~[were~;was~:;were~]~:* ~R."
                                               (count expected)
                                               (count actual)))))))))
                            
(defchecker has [quantifier predicate]
  (checker [actual]
    (quantifier predicate
                (if (map? actual)
                  (vals actual)
                  actual))))

;; These are used in some internal tests. Worth publicizing?

(defchecker n-of [expected expected-count]
  (chatty-checker [actual]
    (and (= (count actual) expected-count)
         (every? #(extended-= % expected) actual))))

(defmacro ^:private generate-n-of-checkers []
  (macro-for [[int checker-name] [[1 "one"] [2 "two"] [3 "three"] [4 "four"] [5 "five"]
                                  [6 "six"] [7 "seven"] [8 "eight"] [9 "nine"] [10 "ten"]]]
    `(defchecker ~(symbol (str checker-name "-of")) [expected-checker#]
       (n-of expected-checker# ~int))))

(generate-n-of-checkers)