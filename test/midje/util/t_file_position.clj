(ns midje.util.t-file-position
  (:use [midje.util.file-position] :reload-all)
  (:use [midje.semi-sweet :only [fake]])
  (:use [clojure.test])
  (:use [midje.test-util])) 

(def line-marker-1 7)
(deftest simple-user-file-position-test []
  (let [position (user-file-position)]
    (is (= "t_file_position.clj" (first position)))
    (is (= (+ 2 line-marker-1) (second position))))
)

(def line-marker-2 (+ line-marker-1 7)) (declare f)
(deftest one-level-macro-file-position-test []
  (let [fake (fake (f 1) => 33)
	position (:file-position fake)]
    (is (= "t_file_position.clj" (first position)))
    (is (= (+ 2 line-marker-2) (second position))))
)


(defmacro expander [& forms]
  (second forms))

(def line-marker-3 (+ line-marker-2 12))
(deftest one-level-simple-macro-file-position-test []
  (let [fake (expander
		      "random garbage"
		      (fake (f 1) => 33)
		      "more garbage")
	position (:file-position fake)]
    (is (= "t_file_position.clj" (first position)))
    (is (= (+ 4 line-marker-3) (second position))))
)


(defmacro substitutor [& forms]
  `(do
     (fake ~(nth forms 1) => ~(nth forms 3))))


(def line-marker-4 (+ line-marker-3 17))
(deftest one-level-substitutable-macro-file-position-test []
  (let [fake (substitutor        ; this is the expected line number
		      "random garbage"
		      (f 1) => 33)
	position (:file-position fake)]
    (is (= "t_file_position.clj" (first position)))
    (is (= (+ 2 line-marker-4) (second position))))
)

(deftest simple-user-file-position-test
  (let [position (line-number-known 33)]
    (is (= "t_file_position.clj" (first position)))
    (is (= 33 (second position)))))

