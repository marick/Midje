(ns midje.unprocessed.t-unprocessed-background-fakes
  (:use clojure.test)
  (:use [midje.unprocessed.background])
  (:use [midje.semi-sweet] :reload-all) ;; test indirectly
  (:use [midje.test-util]))


(unfinished unused used)
(defn calls-nothing [] )

(deftest background-fakes-need-not-be-called
  (expect (calls-nothing) => nil
	  (fake (unused) => 3 :type :background)))

(deftest pushing-and-popping
  (is (empty? (background-fakes)))
  (let [fakes [(fake (unused) => 3) (fake (used) => 4)]
	more-fakes [ (fake (calls-nothing) => 5) ]]
    (push-background-fakes fakes)
    (is (= [ (reverse fakes) ] (background-fakes)))
    (push-background-fakes more-fakes)
    (is (= [(reverse more-fakes) (reverse fakes)] (background-fakes)))
    (pop-background-fakes)
    (is (= [(reverse fakes)] (background-fakes)))
    (pop-background-fakes)
    (is (empty? (background-fakes)))))



(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(deftest implicit-use-of-background-fakes
  (push-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)])
  (expect (calls-used) => "hi mom"
	  (fake (local) => "mom"))
  (pop-background-fakes))

(deftest background-wrapper
  (with-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)]
    (expect (calls-used) => "hi mom"
	    (fake (local) => "mom")))
  (is (empty? (background-fakes))))

(deftest background-wrapper-unwind-protects
  (try
    (with-background-fakes [(fake (unused) => 3 :type :background)
			    (fake (used) => "hi" :type :background)]
      (is (not (empty? (background-fakes))))
      (throw (Exception.)))
    (catch Exception ex (is (empty? (background-fakes))))))

