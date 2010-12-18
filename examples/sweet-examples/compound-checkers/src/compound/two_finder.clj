(ns compound.two-finder
  (use midje.sweet))

;;Source

(def count-atom (atom 0))

(defn two-finder
  "filters N elements in input collection and sets count-atom to number found"
  [pred coll]
  (let [filtered (filter pred coll)]
    (swap! count-atom (constantly (count filtered)))
    filtered))

(defn two-finder-bad-list [pred coll]
  (let [filtered (filter pred coll)]
    (swap! count-atom (constantly (count filtered)))
    ["SO WRONG!"]))

(defn two-finder-bad-atom [pred coll]
  [pred coll]
  (let [filtered (filter pred coll)]
    (swap! count-atom (constantly 900))
    filtered))


;;Simple function - easy, but not recommended

(defn finds [expected]
  (fn [actual]
    (and (= actual expected)
	 (= @count-atom (count expected)))))

(fact
  (two-finder-bad-list odd? [1 2 3]) => (finds [1 3])
  (two-finder-bad-atom odd? [1 2 3]) => (finds [1 3]))

;; Virtues:
;; * Easy to remember how to do it.

;; Flaws:
;; * You only find out that something failed, not which check:
;;         Actual result: (1 3)
;;     Checking function: (finds [1 3])
;;   At first glance, the above surely looks as if it should succeed.


;; Chatty checkers

;; Easy to create by changing the 'fn' to chatty-checker:

(defn finds [expected]
  (chatty-checker [actual]
    (and (= actual expected)
	 (= @count-atom (count expected)))))

(fact
  (two-finder-bad-list odd? [1 2 3]) => (finds [1 3])
  (two-finder-bad-atom odd? [1 2 3]) => (finds [1 3]))

;; Virtues:
;; * Tells you intermediate results:
;;      FAIL at (two_finder.clj:56)
;;      Actual result did not agree with the checking function.
;;          Actual result: (1 3)
;;      Checking function: (finds [1 3])
;;      During checking, these intermediate values were seen:
;;         (= actual expected) => true
;;         (= (clojure.core/deref count-atom) (count expected)) => false

;; Flaws:
;; * Chatty checkers currently don't obey short-circuiting AND,
;;   so second check will be evaluated even if first was false.
;; * More verbose output.
;; * Only works if the checks you're interested fit into a simple form:
;;      (f ..check.. ..check..)



;;A macro containing expects

;; #'fact is a macro that expands out into a "semi-sweet" macro named
;; #'expect. You can use #'expect directly:

(defmacro finds [expected]
  `(fn [actual#]
     (and (expect actual# => ~expected)
	  (expect @count-atom => (count ~expected)))
     true))

;; Explanation:
;; * You have to use a macro so that line numbers can be found correctly.
;; * Because expect returns false if the check fails, you can avoid
;;   redundant checks.
;; * The ending true is to that a failure isn't forwarded on to the check
;;   in the calling form. 

(fact
  (two-finder-bad-list odd? [1 2 3]) => (finds [1 3])
  (two-finder-bad-atom odd? [1 2 3]) => (finds [1 3]))

;; Virtues: 
;; * Terser output than chatty-checkers and works when they don't.

;; Flaws:
;; * It messes up totals. For example, the second check above looks
;;   like one check that fails, but it's actually two succeeding
;;   checks and one failing. (There are two checks within the macro
;;   and one outside. The one outside will always succeed - it's really
;;   there only for decoration.
;; * Harder to remember how to make it.


;; One final stylistic point

;; The various versions of #'finds say nothing to the reader about the
;; count, so this is probably preferable:

(defmacro finds [expected-coll _ expected-count]
  `(fn [actual#]
     (and (expect actual# => ~expected-coll)
	  (expect @count-atom => ~expected-count))
     true))

(fact
  (two-finder-bad-list odd? [111 222 333]) => (finds [111 333] :stashes 2)
  (two-finder-bad-atom odd? [111 222 333]) => (finds [111 333] :stashes 2))

;; Notice that I also changed the numbers in the list to avoid a
;; misinterpretation like "Ah, it stashes the number (numbers?) that
;; were excluded from the result."
