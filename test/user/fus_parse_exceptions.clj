(ns user.fus-parse-exceptions
  (:use midje.sweet
        midje.test-util))


;;; In general, it would be nice if these could be moved over to t-parse-errors.

(silent-fact "an intentional error to demonstrate the mechanism" 
  (fact
    (fact
      (let [a 88]
        (+ 1 2) =throw-parse-exception=> a))))
(note-that parser-threw-exception
           (exception-message-was #"A test asked for an exception")
           (exception-was-for-form-matching #"fact.*fact.*a 88.*throw-parse-exception=> a"))
