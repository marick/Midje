;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-file-position
  (:use [midje.util.file-position]
        [midje sweet test-util]
        [midje.arrows :only [is-start-of-arrow-sequence?]])
  (:require [clojure.zip :as zip]))

(defn this-file [line-number] 
  ["t_file_position.clj" line-number])

;; Throughout this file, file positions are captured outside of
;; facts. That's because facts have their own mechanism for file
;; position, and I want it to be clear that this is just working with
;; the base (utility) function.

;; TODO: The use of user-file-position is a historical relic because semi-sweet mode
;; came before sweet-mode and its use of the alternate method of determining file position.
;; That alternate could be back-ported to (fake) and (expect) -- it's less accurate, though.

(def line-marker-1 21)
(let [position (user-file-position)]
  (fact "you can capture the filename/linenumber of a code line"
    position => (this-file (+ line-marker-1 1))))

(def line-marker-2 (+ line-marker-1 5))
(unfinished f)
(let [fake-on-one-line (fake (f 1) => 33)
      multiline-with-position-at-first-token (fake

                                              (f 1)

                                              =>

                                              33)]
  (fact "fake, being a one-level macro, knows its file position as a single line"
    (:position fake-on-one-line) => (this-file (+ 2 line-marker-2))
    (:position multiline-with-position-at-first-token) => (this-file (+ 3 line-marker-2))))

(defmacro result-of-second-form [& forms] (second forms))

(def line-marker-3 (+ line-marker-2 16))
(let [fake (result-of-second-form
            "random garbage"
            (fake (f 1) => 33)
            "more garbage")]
  (fact "Macros within dirt-simple macroexpansions find their correct file position"
    (:position fake) => (this-file (+ 3 line-marker-3))))

(defmacro fake-constructor [& forms]
  `(do
     (fake ~(nth forms 1) => ~(nth forms 3))))


(def line-marker-4 (+ line-marker-3 13))
(let [fake (fake-constructor        ; sadly, this is the expected line number
                      "random garbage"
                      (f 1) => 33)] ; though we wish it were this.
  (fact "macros that construct fakes won't find user-file-position useful."
    (:position fake) => (this-file (+ 1 line-marker-4))))


;; Macros like the above will need to calculate the file position themselves, but
;; the filename will be valid.
(fact "line-number-known is used when you know the line but not the file"
  (let [position (line-number-known 33)]
    position => ["t_file_position.clj", 33]))

(facts "about determining a line number from forms near an arrow"
  "Typical case is form on left. (f 1) => 5"
  (let [form `( ~(at-line 33 '(f 1)) => 5)
        loc (-> form zip/seq-zip zip/down)]
    loc => is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "When form on the left is has no line, check right: ...a... => (exactly 1)"
  (let [form `( ...a... => ~(at-line 33 '(exactly 1)))
        loc (-> form zip/seq-zip zip/down)]
    loc => is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "If both sides have line numbers, the left takes precedence: (f 1) => (exactly 1)"
  (let [form `( ~(at-line 33 '(f 1)) => ~(at-line 34 '(exactly 1)))
        loc (-> form zip/seq-zip zip/down)]
    loc => is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "If neither side has a line number, look to the left and add 1: (let [a 2] a => b)"
  (let [form `( (let ~(at-line 32 '[a 2]) a => b))
        loc (-> form zip/seq-zip zip/down zip/down zip/right zip/right)]
    loc => is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "Default result is is one plus the fallback line number."
  (set-fallback-line-number-from (at-line 333 '(previous form)))
  (let [form '(1 => 2)
        loc (-> form zip/seq-zip zip/down)]
    loc => is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 334

    ;; incrementing happens more than once
    (arrow-line-number (zip/right loc)) => 335


    (let [another-form `( ~(at-line 3 '(f 1)) => 5) ]
      (-> another-form zip/seq-zip zip/down zip/right arrow-line-number)
      (arrow-line-number (zip/right loc)) => 4)))

(facts "about finding the arrow-line-number from a form"
  (let [form `( ~(at-line 333 '(f 1)) => 3)]
    (arrow-line-number-from-form form) => 333))

(facts "about compile-time discovery of positions and line numbers from a form"
  (form-position (with-meta '(form) {:line 332}))
  => ["midje/util/t_file_position.clj" 332])
                   
