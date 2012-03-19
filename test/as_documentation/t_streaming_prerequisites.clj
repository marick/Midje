(ns as-documentation.t-streaming-prerequisites
  (:use midje.sweet
        midje.util
        midje.test-util
        midje.error-handling.exceptions))
  

;; A typical called/caller relationship. The key thing is that
;; the caller calls the called more than once.
(defn number [] )
(defn two-numbers [] 
  (+ (number) (number)))


;; This demonstrates the ordinary use of Midje. As with Clojure
;; itself, we assume idempotency: the called function will always
;; produce the same value.
(fact "a number can be doubled"
  (two-numbers) => 6
  (provided
    (number) => 3))

;; But what if the called function is stateful in some way? In that
;; case, successive calls can produce different values. That's
;; implemented with the =streams=> arrow:

(fact "You can stream a sequence of values"
  (two-numbers) => 7
  (provided
    (number) =streams=> [3 4]))

;; The idea of streaming values naturally suggests that the
;; right-hand-side of =streams=> could be a lazy sequence, with just
;; enough values created as they are consumed.

(fact "That sequence of values can be a lazy sequence"
  (two-numbers) => 1
  (provided
    (number) =streams=> (range)))

;; You could imagine a test in which the programmer said (1) the
;; function-under-test consumes values from a lazy sequence, but (2)
;; only a fixed number of them. That could look like this:

(fact "Two values are consumed"
  (two-numbers) => 1
  (provided
    (number) =streams=> (range) :times 2))

;; When the right-hand-side is an explicit list or vector of values,
;; the values are not evaluated until asked for.

(def log (atom []))
(defn log-and-return [n] ; 3 of these will be listed, but only two consumed.
  (swap! log conj n)
  n)

(fact "only two stream elements will be evaluated"
  (against-background (before :facts (reset! log [])))
  (two-numbers) => 3
  (provided
    (number) =streams=> [ (log-and-return 1)
                          (log-and-return 2)
                          (log-and-return 3) ])
  @log => [1 2])

;; We want to be gracious about errors, so it should be that asking
;; for the n+1th value when there are only N fails helpfully:

(def useful-message #"Your =stream=> ran out of values")

(run-silently 
 (fact
   (two-numbers) => 2
   (provided
     (number) =streams=> [1])))

(fact
  @reported => has-bad-result
  @reported => (has-thrown-message useful-message))


;; You can stream strings as seqs of characters
(unfinished a-char)

(defn two-chars [] (list (a-char) (a-char)))

(fact
  (two-chars) => [\1 \2]
  (provided (a-char) =streams=> "12"))




;;; Things that go without saying (though not without testing)

;; The use of :times with a stream applies also to explicitly-named
;; stream values:

(fact "Two values are consumed"
  (two-numbers) => 7
  (provided
    (number) =streams=> '[3 4 5 6 7 8] :times 2))

;; The :times case can also fail
(run-silently
 (fact
   (two-numbers) => 7
   (provided
     (number) =streams=> '[3 4 5 6 7 8] :times 1)))

(fact
  @reported => has-wrong-call-count)

;; You can use a quoted list on the right-hand-side, and it works just
;; as with a vector. This test demonstrates how list elements are also
;; not evaluated until needed.

(fact "only two stream elements will be evaluated"
  (against-background (before :facts (reset! log [])))
  (two-numbers) => 3
  (provided
    (number) =streams=> '( (log-and-return 1)
                           (log-and-return 2)
                           (log-and-return 3) ))
  @log => [1 2])


;; Lazy sequences that run out of values generate the
;; same error message as non-lazy sequentials.

(run-silently 
 (fact
   (two-numbers) => 2
   (provided
     (number) =streams=> (range 1 2))))

(fact
  @reported => has-bad-result
  @reported => (has-thrown-message useful-message))



;;;; This does not work

(unfinished a-try)

(defn a-try []
  (   (fn [] (throw (Exception. "fooooooo"))) ))

(defn msg []
  (try
    (a-try)
    (catch Exception ex
      (prn "#'msg going to return " (.getMessage ex))
      (.getMessage ex))))


(defn longest-msg []
  (reduce (fn [so-far text]
            (prn "longest-msg working with " text)
            (if (> (count text) (count so-far))
              text
              so-far))
            [(msg) (msg) (msg)]))

; (prn [(msg) (msg) (msg)])

; (prn (longest-msg))
  
(future-fact "streaming exceptions actually works"
  (longest-msg) => "i am long"
  (provided
    (a-try) =streams=> [ (throw (Exception. "short"))
                         (throw (Exception. "shorter"))
                         (throw (Exception. "i am long")) ]))

