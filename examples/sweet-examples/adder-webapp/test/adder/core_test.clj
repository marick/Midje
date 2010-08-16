(ns adder.core-test
  (:use midje.sweet)
  (:use adder.core))

(fact "parse input checks errors"
 (parse-input "1" "2") => [1 2]      
 (parse-input "foo" "2") => (throws NumberFormatException)
)

(defn contains [regexp] (partial re-find regexp))

(fact  ;; Description is not required.
 (view-output 1 2 3) => (contains #"two numbers added")
)

(let [response (handler {:uri "/" :request-method :get})]
  (fact "Handles bound symbols as you'd expect"
   (:status response) => 200
   (:body response) => (contains #"add two numbers"))
)

;; Approximately one zillion facts will need to check both the
;; response code and the content of the resulting body. You'll want
;; helper functions. Here's one, but it's only virtue is that it does
;; fail and succeed when it should.

(defn awkward-successful-page? [response regexp]
  (and (= (:status response) 200)
       ((contains regexp) (:body response))))

(fact
 (awkward-successful-page? (handler {:uri "/" :request-method :post :params {"a" "1" "b" "2"}})
		   #"1 \+ 2 = 4") => truthy
		   )

(println "^^^^ Failure expected. (The test was changed to test the wrong thing.) ^^^^")

;; The failure messages are not particularly useful. To get better
;; ones, place expect statements within the helper. I think it's more
;; readable to use the helper on the right-hand-side of the statement.

(defn successful-page-containing [regexp]
  (fn [response] 
    (expect (:status response) => 200)
    (expect (:body response) => (contains regexp))
    true))

(let [response (handler {:uri "/" :request-method :post :params {"a" "2" "b" "3"}})]
  (fact response => (successful-page-containing #"those are not both numbers")))

;; The true at the end of the helper is because expect returns false
;; in the case of a failure, which would cause a cascading failure in
;; the caller.

(println "^^^^ Failure expected. (The test was changed to test the wrong thing.) ^^^^")

(let [response (handler {:uri "/anything" :request-method :get})]
  (fact
   (:status response) => 302
   (get-in response [:headers "Location"]) => "/")
)
  
