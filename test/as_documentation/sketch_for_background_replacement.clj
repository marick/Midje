(ns as-documentation.sketch-for-background-replacement
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))


;;; THESE ARE SKETCHES TOWARD A POSSIBLE FUTURE FEATURE.

;; (comment


;; ;;; how to share prerequisites from other namespaces. Define a
;; ;;; function and call it.

;; (comment

;;   (fact "something"
;;     (assume-prerequisites (ping 1) => 200
;;                           (ping "1") => 200
;;                           (ping 'symbol) => 404)
;;     (f 3) => 5
;;     (provided ...))

;;   (prerequisite-group [(only-numberish-values-succeed?) => true]
;;     (ping 1) => 200
;;     (ping "1") => 200
;;     (ping 'symbol) => 404)

;;   (fact "text"
;;     (assume-prerequisite (only-numberish-values-succeed?) => true)
;;     ...)



;;   (prerequisite-group [(numberish-value-status) => ?desired-status]
;;     (ping 1) => ?desired-status
;;     (ping "1") => ?desired-status
;;     (ping 'symbol) => 404)

;;   (fact "text"
;;     (assume-prerequisite (numberish-value-status) => 200)
;;     ...)


;;   (to-ensure [(log-contents) => ?checker]
;;     (case ?checker
;;       empty?        (reset! log [])
;;       (contains 5)  (reset! log [1 2 5])))

;;   (to-ensure [(log-contents) => ?checker]
;;     (reset! log []))
;;   (to-ensure [(log-contents) => (contains ?value)]
;;     (reset! log (range 0 ?value)))


;;   (fact
;;     (ensure-prerequisite (horse-count) => 5
;;                          (cow-count) => 5)
;;     ...)

;;   (to-ensure [(horse-count) => ?number]
;;     (ensure-prerequisite (animal-count :species :horse) => ?number))


;;   (to-ensure [(animal-count :species ?species) => ?number]
;;     (dotimes [n ?number]
;;       (create-animal :species ?species
;;                      :name  (str "horse-" n))))



;;   (to-ensure [(horses

;;   (to-ensure [@log => empty?]
;;     (reset! log []))

;;   (fact
;;     (ensure-prerequisite @log => empty?)
;;   ;; Empty the log before every example
;;   )


;;   (fact
;;     (f 1) => 2  ; log in undefined state
;;     (ensure-prerequisite @log => empty?)
;;     (f 1) => 2)  ; log is forced to be empty
;;     ...)



;;   (fact
;;     (ensure-prerequisite-here @log => empty?))


;;   (fact "big claim"
;;     (assume-prerequisite (outer 1) => 5)
;;     (fact "subclaim"
;;       (assume-prerequisite (inner 1) => 5)
;;       ;; examples here see both the inner and outer prerequisites
;;       )
;;     ;; An example here would see only the outer prerequisite.
;;     (fact "another subclaim")
;;       (assume-prerequisite (inner 1) => "55555555555555555555")
;;       ;; examples here see both prerequisites, but note that
;;       ;; `(inner 1)` returns a different value.
;;       )

;; )

;; (fact "you can insert into the database"
;;   (ensure-prerequisite-here (table :table) => empty?)
;;   (insert :table, :greeting "hi", :person "mom!") => truthy
;;   (let [matches (select :table, :greeting "hi")
;;         match (first matches)]
;;     (count matches) => 1
;;     (:greeting match) => "hi"
;;     (:person match) => "mom!"))

;; )
