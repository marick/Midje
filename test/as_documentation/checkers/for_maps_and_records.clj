(ns as-documentation.checkers.for-maps-and-records
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))


                                ;;; Implications of extended equality

(defrecord R [x y])
(defrecord NotR [x y])

;; Map and record comparisons.

;; When you put a map on the right-hand side, you're declaring that you care about
;; only *contents*, not type. Therefore, a map can match a record.

(fact (R. 1 2) => {:x 1, :y 2})

;; When you  put a record on the right-hand side, you declare that you care about
;; *both* contents and type. Content-equality is not enough.

(silent-fact
  {:x 1, :y 2} => (R. 1 2)
  (NotR. 1 2)  => (R. 1 2))
(note-that (failed 2 :times))
(for-failure 1 (note-that (fact-failed-with-note "A record on the right of the arrow means the value on the left must be of the same type.")))
(for-failure 2 (note-that (fact-failed-with-note "A record on the right of the arrow means the value on the left must be of the same type.")))

;; The =not=> arrow is a tricky case. Consider this:
;;
;; (fact
;;   {:x 1, :y 2} =not=> (R. 1 2}
;;
;; That fact passes because indeed the map is not extended-= to a record. But there is no
;; situation in which such a fact could *ever* fail. So it's of no help detecting what is
;; likely to be a bug: that the actual value is a map. So =not=> will *also* fail whenever
;; the left-hand-side is a map and the right-hand side is a record.

(silent-fact
  {:x 1, :y 2} =not=> (R. 1 2)
  (NotR. 1 2)  =not=> (R. 1 2))
(note-that (failed 2 :times))
(for-failure 1 (note-that (fact-failed-with-note "A record on the right of the arrow means the value on the left must be of the same type.")))
(for-failure 2 (note-that (fact-failed-with-note "A record on the right of the arrow means the value on the left must be of the same type.")))

;; =not=> comparisons will only be made between records of the same type:

(fact (R. 1 2) =not=> (R. 1 3333333))
(silent-fact (R. 1 2) =not=> (R. 1 2))
(note-that fact-failed)




;;; Here are some examples of using collection checkers with maps and records.

(fact "`contains` and `just` work on key-value pairs"
  {:x 1, :y 'IGNORE} => (contains {:x 1})
  (R. 1 'IGNORE!) => (contains {:x 1})

  {:x 1, :y 'IGNORE} =not=> (just {:x 1})
  (R. 1 'IGNORE!) =not=> (just {:x 1}))

(fact "checker functions provide extended equality"
  (R. 1 'IGNORE) => (contains {:x odd?})
  {:a 1, :b 2, :c "some text"} => (just {:a odd?, :b 2, :c #"text"})
  {:a 1, :b 3} => (has every? odd?))

(silent-fact "The rules for the right-hand side described above also apply to these checkers."
  {:a 1, :b 2} => (just (R. 1 2))
  (NotR. 1 2) => (just (R. 1 2)))
(note-that (failed 2 :times))
(for-failure 1 (note-that (fact-failed-with-note "A record on the right of the arrow means the value on the left must be of the same type.")))
(for-failure 2 (note-that (fact-failed-with-note "A record on the right of the arrow means the value on the left must be of the same type.")))

(future-fact "At the moment, you don't get the failure for the =not=> case."
  {:a 1 :b 2} =not=> (just (R. 1 2)))
;; (note-that (fact-failed-with-note #"expected.*R but the actual value was a map"))


;;; If you want to use `contains` but also insist on an exact type, use a combining checker:

(fact
  (R. 1 'IGNORE!) => (every-checker #(instance? R %)
                                    (contains {:x 1}))
  {:x 1, :y 2} =not=> (every-checker #(instance? R %)
                                     (contains {:x 1}))
  (R. 2 'IGNORE) =not=> (every-checker #(instance? R %)
                                       (contains {:x 1})))

(fact "ways to make claims about keys"
  (keys {:x 1, :y 1}) => (just #{:x :y})            ;; Contains every key
  {:x 1, :y 1} => (just {:x anything, :y anything}) ;; a variant

  (keys {:x 1, :y 1}) => (contains #{:x}) ;; Contains some of the keys
  {:x 1, :y 1} => (contains {:x anything}))



                                ;;; key/value pairs

(fact "a sequence of key/value pairs is OK on the right-hand side"
  {:a 1, :b 2} => (just [[:a 1] [:b 2]])
  (R. 1 nil) => (contains [[:x 1]]))
