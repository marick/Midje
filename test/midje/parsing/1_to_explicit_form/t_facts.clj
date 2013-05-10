(ns midje.parsing.1-to-explicit-form.t-facts
  (:use midje.parsing.1-to-explicit-form.facts
        midje.sweet midje.test-util)
  (:require [clojure.zip :as zip]
            [midje.config :as config]))



;; Translating sweet forms into their semi-sweet equivalent


(fact "translating entire fact forms"
  "some parts of a fact are to be left alone"
  (let [form '(a-form-would-go-here another-would-go-here)]
    (to-semi-sweet form) => form)

  (let [form '( (nested (form) form ) [ 1 2 3])]
    (to-semi-sweet form) => form)

  "arrow sequences are wrapped with expect"
  (let [form '(                              (f 1) => [2]                           (f 2) => (+ 1 2) )
        expected '( (midje.semi-sweet/expect (f 1) => [2]) (midje.semi-sweet/expect (f 2) => (+ 1 2)))]
    (expect (to-semi-sweet form) => expected))

  "the wrapping can include prerequisites turned into fake forms."
  (let [form '( (f 1) => [1] :ekey "evalue"
                (f 2) => (+ 2 2)
                (provided (g 3) => 3
                          (g 4) => 4 :pkey "pvalue")
                (f 5) => truthy)
        expected '( (midje.semi-sweet/expect (f 1) => [1] :ekey "evalue")
                    (midje.semi-sweet/expect (f 2) => (+ 2 2)
                                             (midje.semi-sweet/fake (g 3) => 3)
                                             (midje.semi-sweet/fake (g 4) => 4 :pkey "pvalue"))
                    (midje.semi-sweet/expect (f 5) => truthy))]
    (to-semi-sweet form) => expected)

  "It's useful to embed expect clauses with notcalled prerequisites, so they're skipped"
  (let [form '(    (expect (f 1) => 2 (fake (g 1) => 2))
                                      (fake (m 1) => 33))]
    (to-semi-sweet form) => form))

(config/with-augmented-config {:check-after-creation false}
  (with-out-str (fact 1 => 2)) => "")


(fact "one can add a line number to an arrow sequence"
  (let [original '( (f n) => 2  )
        expected '( (f n) => 2 :position (midje.parsing.util.file-position/line-number-known 10))
        z            (zip/seq-zip original)
        original-loc (-> z zip/down zip/right)
        new-loc      (at-arrow__add-line-number-to-end__no-movement
                        10 original-loc)]
    (name (zip/node new-loc)) => "=>"
    (zip/root new-loc) => expected))


(fact "a whole form can have line numbers added to its arrow sequences"
  (let [original `(let ~(with-meta '[a 1] {:line 33})
                    a => 2
                    ~(with-meta '(f 2) {:line 35}) => a)
        actual (annotate-embedded-arrows-with-line-numbers original)
        expected '(clojure.core/let [a 1]
                                    midje.parsing.1-to-explicit-form.t-facts/a midje.sweet/=> 2 :position (midje.parsing.util.file-position/line-number-known 34)
                                    (f 2) midje.sweet/=> midje.parsing.1-to-explicit-form.t-facts/a :position (midje.parsing.util.file-position/line-number-known 35))]
    actual => expected))

(fact "various arrow forms have line numbers"
  (let [original `(
                    (~(with-meta '(f 1) {:line 33}) => 2)
                    (~(with-meta '(f 1) {:line 33}) =not=> 2)
                    (~(with-meta '(f 1) {:line 33}) =streams=> 2)
                    (~(with-meta '(f 1) {:line 33}) =future=> 2))
        actual (annotate-embedded-arrows-with-line-numbers original)]
    (doseq [expansion actual]
      (take-last 2 expansion)
      => '(:position (midje.parsing.util.file-position/line-number-known 33)))))

(fact "Issue #117 - arrows inside quoted forms will not have :position info added"
  '(fact foo => bar) => '(fact foo => bar))

(fact "facts can be unparsed"
  (let [result (wrap-fact-around-body {:midje/source "s", :midje/guid "guid", :midje/line 8888888
                                       :integration true, :midje/name "fred"} '((form1) (form2)))]
    (fact "it is a fact form"
      result => (contains ['midje.sweet/fact '(form1) '(form2)] :gaps-ok))
    (fact "it contains the source and guid"
      (some #(and (map? %) (:midje/source %)) result) => "s"
      (some #(and (map? %) (:midje/guid %)) result) => "guid")
    (fact "it does not contain other midje core metadata"
      (not-any? #(and (map? %) (:midje-line %)) result) => truthy)
    (fact "it carries forward user-supplied metadata"
      (some #(and (map? %) (:integration %)) result) => true
      (some #(= % 'fred) result) => truthy)))
  

