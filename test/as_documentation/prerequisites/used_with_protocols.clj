(ns as-documentation.prerequisites.used-with-protocols
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.open-protocols :refer :all]))

;;; Clojure inlines deftype and defrecord functions for speed. Consider this protocol and record:

(defprotocol Addable
  (add-fields [this]))

(defrecord MyRecord [a b]
  Addable
  (add-fields [this] (+ a b)))

;; Although add-fields is now a defined var...

(fact (resolve 'add-fields) => truthy)

;; ... you cannot use it as a prerequisite: the changed behavior isn't noticed.

(defn add-fields-producing-a-string [record]
  (str (add-fields record)))

(silent-fact
  (let [rec (MyRecord. 1 2)]
    (add-fields-producing-a-string rec) => "3"
    (add-fields-producing-a-string rec) => "faked!" ;; Still "3" even though we use a `provided`.
    (provided
      (add-fields anything) => "faked!")))
(note-that (failed 2 :times))
(for-failure 1 (note-that (prerequisite-was-called-the-wrong-number-of-times #"add-fields anything" 0 :times)))
(for-failure 2 (note-that (fact-actual "3") (fact-expected "faked!")))


;; In order to override the default add-fields, we have to fake out the inlining. That's done with the
;; defrecord-openly form:

(defrecord-openly MyRecord [a b]
  Addable
  (add-fields [this] (+ a b)))

;; Now the prerequisite works:

(fact
  (let [rec (MyRecord. 1 2)]
    (add-fields-producing-a-string rec) => "3"
    (add-fields-producing-a-string rec) => "faked!"
    (provided
      (add-fields anything) => "faked!")))

;; You can also use the "-openly" suffix with deftypes. There's an example below.


;;;                                     Production mode

;; When Midje is in "production mode", `defrecord-openly` does the same thing as `defrecord`, so
;; the speed gains of inlining remain:

(alter-var-root #'include-midje-checks (constantly false))

(defrecord-openly MyRecord [a b]
  Addable
  (add-fields [this] (+ a b)))

(alter-var-root #'include-midje-checks (constantly true))

(silent-fact
  (let [rec (MyRecord. 1 2)]
    (add-fields-producing-a-string rec) => "3"
    (add-fields-producing-a-string rec) => "faked!" ;; Still "3" even though we use a `provided`.
    (provided
      (add-fields anything) => "faked!")))
(note-that (failed 2 :times))
(for-failure 1 (note-that (prerequisite-was-called-the-wrong-number-of-times #"add-fields anything" 0 :times)))
(for-failure 2 (note-that (fact-actual "3") (fact-expected "faked!")))

;; Note that you have to use `alter-var-root` instead of
;; `binding`. Clojure macroexpands before it establishes the binding,
;; so a wrapping form doesn't work. Normally, you'd put the
;; `alter-var-root` in a configuration file anyway.



;;;                             Records that implement Java interfaces

;;; Records and types can also implement methods from Java interfaces (and override
;;; methods of Object). However, those are still implemented as Java methods, so they
;;; cannot be overridden with prerequisites.

(defprotocol Fearful
  (fear? [this]))

(deftype-openly Fear []
  Fearful
  (fear? [this] true)
  Comparable
  (compareTo [this that] -1)    ;; "-openly" has no effect.
  Object
  (toString [this] "object"))   ;; "-openly" has no effect.

(fact "The Java functions are harmlessly skipped."
  (fear? (Fear.)) => true
  (.compareTo (Fear.) :anything) => -1
  (.toString (Fear.)) => "object")

(fact "Only the protocol function can be overridden."
  (resolve 'compareTo) => falsey
  (resolve 'toString) => falsey
  (let [fear (Fear.)]
    (fear? fear) => :faked
    (provided
      (fear? fear) => :faked)))



                                        ;;; An example used in the user documentation
(defprotocol Peanoific
  (pzero? [this])
  (pequal? [this that])
  (psuccessor [this])
  (padd [this that])
  (pmult [this that]))

(defrecord-openly Peano [representation]
  Peanoific
  (pzero? [this] :unfinished)
  (pequal? [this that] :unfinished)
  (psuccessor [this] :unfinished)
  (padd [this that]
     (if (pzero? that)
         this
         :unfinished))
  (pmult [this that] :unfinished))


(fact
  (padd (Peano. ...n...) (Peano. ...zero...)) => (Peano. ...n...)
  (provided
    (pzero? (Peano. ...zero...)) => true))

(silent-fact
 (padd (Peano. ...a...) (psuccessor (Peano. ...b...))) => (psuccessor (padd (Peano. ...a...) (Peano. ...b...)))
 (provided
   (pzero? anything) => true))
(note-that fact-fails, (fact-expected '(psuccessor (padd (Peano. ...a...) (Peano. ...b...)))))



