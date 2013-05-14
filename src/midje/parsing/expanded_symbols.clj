(ns ^{:doc "Lexicon of all symbols at the head of expanded forms."}
  midje.parsing.expanded-symbols)

(def expect 'midje.parsing.2-to-lexical-maps.expects/expect)
(def fake 'midje.parsing.2-to-lexical-maps.fakes/fake)
(def data-fake 'midje.parsing.2-to-lexical-maps.data-fakes/data-fake)

(def all #{expect fake data-fake})
