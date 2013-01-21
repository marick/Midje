(ns midje.parsing.t-metadata
  (:use midje.sweet
        midje.test-util
        midje.parsing.metadata))

(def a-body '((f) => 3))

(facts "about separate-metadata" 
  (fact "contains the original source and other info"
    (let [[meta _] (separate-metadata `(fact "doc" ~@a-body))]
      (:midje/source meta) => `(fact "doc" ~@a-body)
      (:midje/body-source meta) => a-body
      (:midje/file meta) => "midje/parsing/t_metadata.clj"
      (:midje/namespace meta) => 'midje.parsing.t-metadata
      (contains? meta :midje/line) => truthy))

  
  (fact "ignores the head of the form"
    (let [[meta _] (separate-metadata `(FOO "doc" ~@a-body))]
      (:midje/source meta) => `(FOO "doc" ~@a-body)
      (:midje/name meta) => "doc"))
  
  (fact "doc strings"
    (fact "can be separated"
      (let [[meta body] (separate-metadata `(fact "doc" ~@a-body))]
        (:midje/description meta) => "doc"
        body => a-body))
    (fact "need not be present"
      (let [[meta body] (separate-metadata `(fact ~@a-body))]
        (:midje/description meta) => nil
        body => a-body))
    (fact "can provide the name"
      (let [[meta body] (separate-metadata `(fact "doc" ~@a-body))]
        (:midje/name meta) => "doc"
        body => a-body)))
  
  (facts "symbols"
    (fact "become the fact name"
      (let [[meta body] (separate-metadata `(fact cons ~@a-body))]
        (:midje/name meta) => "cons"
        body => a-body))
    (fact "take precedence over strings"
      (let [[meta body] (separate-metadata `(fact "foo" cons ~@a-body))]
        (:midje/name meta) => "cons"
        body => a-body)
      (let [[meta body] (separate-metadata `(fact cons "foo" ~@a-body))]
        (:midje/name meta) => "cons"
        body => a-body))
    (fact "don't count as names when they are the head of an expect form"
      (let [[meta body] (separate-metadata `(fact foo => 3))]
        (:midje/name meta) => nil
        body => `(foo => 3))))

  (fact "keywords become true metadata"
    (let [[meta body] (separate-metadata `(fact :a :b  ~@a-body))]
      (:a meta) => true
      (:b meta) => true
      body => a-body))

  (fact "metadata can be an explicit map"
    (let [[meta body] (separate-metadata `(fact name {:a 1}  ~@a-body))]
      (:midje/name meta) => "name"
      (:a meta) => 1
      body => a-body)))

(facts "about separate-two-level-metadata"
  (let [;; The core structure is this:
        two-level-without-metadata                `(Level1
                                                     (Level2 ~@a-body)
                                                     Other Stuff)

        ;; But it can be annotated with metadata at top
        two-level-original-with-metadata-at-top   `(Level1 name :key "description"
                                                     (Level2 ~@a-body)
                                                     Other Stuff)

        ;; ... or within the first enclosed form.
        two-level-original-with-metadata-at-lower `(Level1
                                                     (Level2 name :key "description"
                                                             ~@a-body)
                                                     Other Stuff)]

    (fact "promotes second-level data"
      (let [[meta body] (separate-two-level-metadata two-level-original-with-metadata-at-lower)]
        meta => (contains {:midje/name "name"
                           :midje/description "description"
                           :key true})
        body => (rest two-level-without-metadata)))
    ;; It is undefined which takes precedence in case of a clash.

    (fact "second-level data is not required"
      (let [[meta body] (separate-two-level-metadata two-level-original-with-metadata-at-top)]
        meta => (contains {:midje/name "name"
                           :midje/description "description"
                           :key true})
        body => (rest two-level-without-metadata)))

    ;; It is undefined which takes precedence in case of a clash between lower and top level.

    (fact "the source is the original"
      (letfn [(source-of [form]
                (:midje/source (first (separate-two-level-metadata form))))]
        (source-of two-level-original-with-metadata-at-top) => two-level-original-with-metadata-at-top
        (source-of two-level-original-with-metadata-at-lower) => two-level-original-with-metadata-at-lower))

    (fact "the body source is stripped of metadata"
      (letfn [(body-source-of [form]
                (:midje/body-source (first (separate-two-level-metadata form))))]
        (body-source-of two-level-original-with-metadata-at-top) => (rest two-level-without-metadata)
        (body-source-of two-level-original-with-metadata-at-lower) => (rest two-level-without-metadata)))))

