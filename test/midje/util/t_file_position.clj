;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-file-position
  (:use [midje.util.file-position])
  (:use [midje.sweet])
  (:use [clojure.test])
  (:use [midje.test-util]))

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
    (:file-position fake-on-one-line) => (this-file (+ 2 line-marker-2))
    (:file-position multiline-with-position-at-first-token) => (this-file (+ 3 line-marker-2))))

(defmacro result-of-second-form [& forms] (second forms))

(def line-marker-3 (+ line-marker-2 16))
(let [fake (result-of-second-form
            "random garbage"
            (fake (f 1) => 33)
            "more garbage")]
  (fact "Macros within dirt-simple macroexpansions find their correct file position"
    (:file-position fake) => (this-file (+ 3 line-marker-3))))

(defmacro fake-constructor [& forms]
  `(do
     (fake ~(nth forms 1) => ~(nth forms 3))))


(def line-marker-4 (+ line-marker-3 13))
(let [fake (fake-constructor        ; sadly, this is the expected line number
                      "random garbage"
                      (f 1) => 33)] ; though we wish it were this.
  (fact "macros that construct fakes won't find user-file-position useful."
    (:file-position fake) => (this-file (+ 1 line-marker-4))))


;; Macros like the above will need to calculate the file position themselves, but
;; the filename will be valid.
(fact "line-number-known is used when you know the line but not the file"
  (let [position (line-number-known 33)]
    position => ["t_file_position.clj", 33]))
