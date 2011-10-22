(ns behaviors.t-macros
  (:use [midje sweet test-util]))

(defmacro macro-with-expander [arg1 & other-args]
  (let [modified (str arg1 "-suffix")]
    `(str ~modified "(was " ~arg1 ")"
          (apply + '~other-args))))

(defn times-ten [arg] (* 10 arg))

(defmacro macro-with-only-expansion [& args]
  `(map times-ten ~@args))

(defmacro macro-calling-other-macro [arg others]
  `(str ~arg (first (macro-with-only-expansion (list ~@others)))))

(facts "about expecting on macro expansion"
       (fact "macros with expansion code only work"
             (macro-with-only-expansion (list 1 2 3)) => (list 10 20 30)
             (macro-with-only-expansion (list 1 2 3)) =expands-to=> (clojure.core/map behaviors.t-macros/times-ten (list 1 2 3)))
       
       (fact "macros with expander code work"
             (macro-with-expander 666 10 20 30) => "666-suffix(was 666)60"
             (macro-with-expander 666 10 20 30) =expands-to=> (clojure.core/str "666-suffix" "(was " 666 ")" (clojure.core/apply clojure.core/+ '(10 20 30))))
       
       (fact "macros calling other macros are macroexpand-1"
             (macro-calling-other-macro 999 [ 10 20 30]) =>  "999100"
             (macro-calling-other-macro 999 [ 10 20 30]) =expands-to=> (clojure.core/str 999 (clojure.core/first (behaviors.t-macros/macro-with-only-expansion (clojure.core/list 10 20 30))))))
