(ns adder.core-test
  (:use midje.sweet)
  (:use adder.core))

(facts "about utilities"
  "parse-input"
  (parse-input "1" "2") => [1 2]      
  (parse-input "foo" "2") => (throws NumberFormatException)

  "view-output"
  (view-output 1 2 3) => (contains #"two numbers added"))

(facts "about the main page"
  "Bare request produces prompt"
  (handler {:uri "/" :request-method :get})
  => (contains {:status 200
                :body #"add two numbers"})

  "Valid numbers produce sum"
  (handler {:uri "/" :request-method :post :params {"a" "1" "b" "2"}})
  => (contains {:status 200
                :body #"1 \+ 2 = 3"})

  "Non-numbers produce error message"
  (handler {:uri "/" :request-method :post :params {"a" "2" "b" "f"}})
  => (contains {:status 200
                :body #"those are not both numbers"}))

(fact "Other pages produce redirect"
  (handler {:uri "/anything" :request-method :get})
  => (contains {:status 302,
                :headers (contains {"Location" "/"})}))
