(ns midje.file_position_test
  (:use [midje.semi-sweet] :reload-all)
  (:use [clojure.test])
  (:use [midje.test-util]))

(def line-marker-1 6)
(deftest simple-user-file-position-test []
  (let [position (user-file-position)]
    (is (= "file_position_test.clj" (first position)))
    (is (= (+ 2 line-marker-1) (second position))))
)

(def line-marker-2 (+ line-marker-1 7))
(deftest one-level-macro-file-position-test []
  (let [expectation (fake (f 1) => 33)
	position (:file-position expectation)]
    (is (= "file_position_test.clj" (first position)))
    (is (= (+ 2 line-marker-2) (second position))))
)


(defmacro expander [& forms]
  (second forms))

(def line-marker-3 (+ line-marker-2 12))
(deftest one-level-simple-macro-file-position-test []
  (let [expectation (expander
		      "random garbage"
		      (fake (f 1) => 33)
		      "more garbage")
	position (:file-position expectation)]
    (is (= "file_position_test.clj" (first position)))
    (is (= (+ 4 line-marker-3) (second position))))
)


(defmacro substitutor [& forms]
  `(do
     (fake ~(nth forms 1) => ~(nth forms 3))))


(def line-marker-4 (+ line-marker-3 17))
(deftest one-level-substitutable-macro-file-position-test []
  (let [expectation (substitutor        ; this is the expected line number
		      "random garbage"
		      (f 1) => 33)
	position (:file-position expectation)]
    (is (= "file_position_test.clj" (first position)))
    (is (= (+ 2 line-marker-4) (second position))))
)

;;; TODO: Perhaps have the higher-level macro try to guess where the correct line is,
;;; based on some formatting conventions. Or emacs command that adds line numbers to file?
