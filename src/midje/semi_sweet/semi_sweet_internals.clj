(ns midje.semi-sweet.semi-sweet-internals
  (:use midje.util.file-position)
)

(defn only-mocked* [names]
  (let [declarations (map (fn [name] 
			      `(defn ~name [& args#] 
				 (throw (Error. (str "#'" '~name " has no implementation. It's used as a prerequisite in Midje tests.")))))
			  names)]
    `(do ~@declarations)))

(defn common-to-all-expectations [var-sym] 
  `{:function (var ~var-sym)
    :count-atom (atom 0)
    :file-position (user-file-position)})

;; In case of multiple keys, the last one takes precedence.
(defn midje-override-map [keys-and-vals]
  (if (empty? keys-and-vals)
    {}
    (apply assoc (cons {} keys-and-vals))))

(defn make-expectation-map 
  [var-sym special-to-expectation-type user-override-pairs]
  `(do
;; Trying out forcing fakes to be declared before use, not *by* use.
;     ~(when-not (resolve var-sym) `(def ~var-sym))
     ~(merge
       (common-to-all-expectations var-sym)
       special-to-expectation-type
       (midje-override-map user-override-pairs)))
)

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn separate [overrides-and-expectations]
  (let [expectation? #(and (seq? %)
			   (or (= "fake" (name (first %)))
			       (= "not-called" (name (first %)))))
	grouped (group-by expectation? overrides-and-expectations)
	default-values {false '() true '()}
	separated (merge default-values grouped)]
    [(separated false) (separated true)])
)

(defmacro call-being-tested [call-form expected-result overrides]
  "Creates a map that contains a function-ized version of the form being 
   tested, an expected result, and the file position to refer to in case of 
   failure. See 'expect'."
  `(merge
    {:function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :expected-result-text-for-failures '~expected-result
     :file-position (user-file-position)}
    (midje-override-map ~overrides)))

