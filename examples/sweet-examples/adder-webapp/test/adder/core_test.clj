(ns adder.core-test
  (:use midje.sweet)
  (:use adder.core))

(facts "about utilities"
  "parse-input"
  (parse-input "1" "2") => [1 2]      
  (parse-input "foo" "2") => (throws NumberFormatException)

  "view-output"
  (view-output 1 2 3) => (contains #"two numbers added"))

;; Approximately one zillion facts will need to check both the
;; response code and the content of the resulting body, so I make
;; a custom checker. For other kinds of custom checkers, see
;; examples/compound-checkers.

(defmacro successful-page-containing [regexp]
  `(fn [response#]
     (and 
      (expect (:status response#) => 200)
      (expect (:body response#) => (contains ~regexp)))
     true))

(facts "about the main page"
  "Bare request produces prompt"
  (handler {:uri "/" :request-method :get})
  => (successful-page-containing #"add two numbers")

  "Valid numbers produce sum"
  (handler {:uri "/" :request-method :post :params {"a" "1" "b" "2"}})
  => (successful-page-containing #"1 \+ 2 = 3")

  "Non-numbers produce error message"
  (handler {:uri "/" :request-method :post :params {"a" "2" "b" "f"}})
  => (successful-page-containing #"those are not both numbers"))

(fact "Other pages produce redirect"
  (let [response (handler {:uri "/anything" :request-method :get})]
    (:status response) => 302
    (get-in response [:headers "Location"]) => "/"))
