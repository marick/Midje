(ns ^{:doc "Handles the parsing of background forms. For the moment, this includes both
            state changes and fact-wide prerequisites."}
  midje.parsing.1-to-explicit-form.parse-background
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        [midje.parsing.1-to-explicit-form.metaconstants :only [predefine-metaconstants-from-form]]
        [midje.parsing.1-to-explicit-form.prerequisites :only [prerequisite-to-fake take-arrow-sequence]]
        [midje.data.prerequisite-state :only [with-installed-fakes]]
        [midje.util.laziness :only [eagerly]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out 
                                                   with-pushed-namespace-values]])
  (:require [clojure.zip :as zip]
            [midje.config :as config]
            [midje.util.unify :as unify]
            [midje.parsing.util.file-position :as position]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.2-to-lexical-maps.fakes :as fakes]
            [midje.parsing.2-to-lexical-maps.data-fakes :as data-fakes]
            [midje.emission.api :as emit]))

(defn background-fakes []
  (namespace-values-inside-out :midje/background-fakes))


;; Dissecting background changers. The TERMINOLOGY file will help you understand this.

(defn separate-extractable-background-changers [fact-body-forms]
  (letfn [(definitely-extractable-form? [form]
            (any? (partial first-named? form) ["prerequisite" "prerequisites"
                                               "background"
                                               "with-state-changes"]))
          (has-wrapper-syntax? [form]
            (and (> (count form) 2)
                 (vector? (second form))))
          (extractable-background-changer? [form]
            (cond (definitely-extractable-form? form) true
                  (not (first-named? form "against-background")) false
                  (has-wrapper-syntax? form) false
                  :else true))]
    (let [[background-forms other-forms] (separate extractable-background-changer? fact-body-forms)
          background-changers            (mapcat (fn [[command & args]] (arglist-undoing-nesting args))
                                                 background-forms)]
      [background-changers other-forms])))


;; Substituting wrapped forms into state changers.

(defn- at-substitution-loc? [loc]
  (symbol-named? (zip/node loc) "?form"))

(defn- substitute-correct-form-variable [form]
  (translate-zipper form
       at-substitution-loc? #(zip/replace % (unify/?form))))

(defmacro before 
  "Code to run before a given wrapping target (:facts, :contents, :checks).
  Can take an optional keyword argument :after, for any code to run afterward.
  Used with background and against-background"
  [_wrapping-target_ before-form & {:keys [after]}]
  (substitute-correct-form-variable `(try
                                       ~before-form
                                       ?form
                                     (finally ~after))))

(defmacro after 
  "Code to run after a given wrapping target (:facts, :contents, :checks).
  Used with background and against-background"
  [_wrapping-target_ after-form]
  (substitute-correct-form-variable `(try ?form (finally ~after-form))))

(defmacro around 
  "Code to run around a given wrapping target (:facts, :contents, :checks).
  Use the symbol '?form' to denote the code that is being wrapped around.
     
  Ex.
  (around :contents (let [a 999] 
                      ?form
                      (print a))) 
     
  Used with background and against-background"
  [_wrapping-target_ around-form]
  (substitute-correct-form-variable around-form))


(defn first-form-could-be-a-state-changer? [forms]
  (and (or (list? (first forms))
           (seq? (first forms)))
       (symbol? (ffirst forms))))

(defn first-form-is-a-state-changer? [forms]
  (#{"before" "after" "around"} (name (ffirst forms))))



(defn- ^{:testable true } extract-background-changers
  ([forms error-reporter]
     (loop [expanded []
            in-progress forms]
       (pred-cond in-progress
                  empty? 
                  expanded
                  
                  recognize/start-of-prerequisite-arrow-sequence?
                  (let [arrow-seq (take-arrow-sequence in-progress)]
                    (recur (conj expanded (-> arrow-seq prerequisite-to-fake fakes/tag-as-background-fake))
                           (drop (count arrow-seq) in-progress)))
                  
                  recognize/metaconstant-prerequisite?
                  (let [arrow-seq (take-arrow-sequence in-progress)]
                    (recur (conj expanded (-> arrow-seq prerequisite-to-fake))
                           (drop (count arrow-seq) in-progress)))
                  
                  (complement first-form-could-be-a-state-changer?)
                  (error-reporter (cl-format nil "~S does not look like a prerequisite or a before/after/around state changer." (first in-progress)))
                  
                  first-form-is-a-state-changer?
                  (recur (conj expanded (first in-progress))
                         (rest in-progress))

                  :else
                  (error-reporter (cl-format nil "~S does not look like a before/after/around code runner." (first in-progress))))))
  ([forms]
     (extract-background-changers forms
                                       (fn [& args]
                                         (throw (Error. "Supposedly impossible error parsing a background changer."))))))

(defn- ^{:testable true } state-wrapper [[_before-after-or-around_ wrapping-target & _ :as state-description]]
  (wrapping/with-wrapping-target
    (macroexpand-1 (map-first #(symbol "midje.parsing.1-to-explicit-form.parse-background" (name %)) state-description))
    wrapping-target))

(letfn [(background-fake-wrappers [fake-maker-forms]
          (let [around-facts-and-checks `(with-pushed-namespace-values
                                           :midje/background-fakes
                                           [~@fake-maker-forms] ~(unify/?form))]
            (list 
             (wrapping/with-wrapping-target around-facts-and-checks :facts))))]

  ;; Collecting all the background fakes is here for historical reasons:
  ;; it made it easier to eyeball expanded forms and see what was going on.
  (defn background-wrappers [background-forms]
    (predefine-metaconstants-from-form background-forms)
    (let [[fakes state-descriptions] (separate recognize/fake? (extract-background-changers background-forms))
          state-wrappers (eagerly (map state-wrapper state-descriptions))]
      (if (empty? fakes)
        state-wrappers
        (concat state-wrappers (background-fake-wrappers fakes))))))

(defn body-of-against-background [[_against-background_ _background-forms_ & background-body :as form]]
  `(do ~@background-body))


(def #^:private misused-content-message
  ["It is meaningless to combine `against-background` or `with-state-changes` and"
   "a `:check-after-creation` configuration variable that's false. If you want some"
   "state to be set up the first time one of the enclosed facts is run, do something"
   "like this:"
   ""
   "user=> (def state (atom nil))"
   "user=> (defn ensure-state []"
   "         (when (nil? @state) "
   "           (println \"One-time setup\")"
   "           (reset! state ...)))"
   ""
   "user=> (against-background [(before :facts (ensure-state))]"
   "         (fact...) "
   "         (fact...))"
   ""
   "user=> (check-facts)"])


(defn against-background-contents-wrappers [[_against-background_ background-forms & _ :as form]]
  (let [result (filter (wrapping/for-wrapping-target? :contents ) (background-wrappers background-forms))]
    (if (empty? result)
      result
      (conj (vec result)
            (substitute-correct-form-variable `(if-not (config/choice :check-after-creation)
                                                 (emit/fail {:type :parse-error
                                                             :notes ~misused-content-message
                                                             :position '~(position/form-position form)})
                                                 ?form))))))

(defn against-background-facts-and-checks-wrappers [[_against-background_ background-forms & _]]
  (remove (wrapping/for-wrapping-target? :contents ) (background-wrappers background-forms)))

(defn surround-with-background-fakes [forms]
  `(with-installed-fakes (background-fakes)
     ~@forms))


;;; Validation

(def #^:private possible-targets #{:facts, :contents, :checks })
(def #^:private target-text ":facts, :checks, or :contents")
(def wrong (partial cl-format nil "`~S`."))

(defn assert-arg-count! [runner count-set & messages]
  (when-not (count-set (count (rest runner)))
    (apply error/report-error runner (wrong runner) messages)))

(defn assert-targets! [runner]
  (when-not (possible-targets (second runner))
    (error/report-error runner (wrong runner)
                        (cl-format nil "Expected the target of `~S` to be ~A." (first runner) target-text))))


(defn assert-valid-before! [runner]
  (assert-arg-count! runner #{2 4} 
                     "`before` has two forms: `(before <target> <form>)` and `(before <target> <form> :after <form>).")
  (assert-targets! runner)
  (when (and (= 5 (count runner))
             (not= (nth runner 3) :after))
    (error/report-error runner (wrong runner)
                        (cl-format nil "Expected the third argument of `before` to be :after."))))

(defn assert-valid-after! [runner]
  (assert-arg-count! runner #{2} "`after` takes a target and a form to run.")
  (assert-targets! runner))
    
(defn assert-valid-around! [runner]
  (assert-arg-count! runner #{2} "`around` takes a target and a form to run.")
  (assert-targets! runner)
  (when-not (re-find #"\?form\W" (str (nth runner 2)))
    (error/report-error runner (wrong runner)
                        "The wrapper must contain `?form`.")))
  

(defn assert-valid-state-changer! [runner]
  (case (name (first runner))
    "before" (assert-valid-before! runner)
    "after" (assert-valid-after! runner)
    "around" (assert-valid-around! runner)))

(defn assert-right-shape!
  "This is concerned only with the background-changers." 
  [form]
  ;; Note: we don't complain if there are no background changers or forms being wrapped,
  ;; as the empty case could be generated by macros.
  (let [changer-args (if (vector? (second form))
                       (second form)
                       (rest form))
        changers (extract-background-changers changer-args (partial error/report-error form (wrong changer-args)))]
    (doseq [changer changers]
      (cond (first-named? changer "fake")
            (fakes/assert-valid! (position/positioned-form changer form))

            (first-named? changer "data-fake")
            (data-fakes/assert-valid! (position/positioned-form changer form))

            :else
            (assert-valid-state-changer! changer)))))

(defonce at-least-one-string-with-this-name-must-be-present #{})

(defn add-midje-fact-symbols [symbols]
  (alter-var-root #'at-least-one-string-with-this-name-must-be-present
                  union
                  (set (map name symbols))))

;; It would be better to check symbols like `midje/fact` than the string "fact";
;; however, all the symbols are duplicated in midje.sweet and midje.repl (because they
;; can be loaded independently). It seems too convoluted to list everything twice, and the
;; worst that can happen from a name clash is that the parse error isn't caught.
(defn assert-contains-facts! [wrapping-background-form]
  (let [possibilities (-<> wrapping-background-form
                           body-of-against-background
                           flatten
                           (filter symbol? <>)
                           (map name <>)
                           set)]
    (when (empty? (intersection possibilities at-least-one-string-with-this-name-must-be-present))
      (error/report-error wrapping-background-form
                          "Background prerequisites created by the wrapping version of"
                          "`against-background` only affect nested facts. This one"
                          "wraps no facts."
                          ""
                          "Note: if you want to supply a background to all checks in a fact, "
                          "use the non-wrapping form. That is, instead of this:"
                          "    (fact "
                          "      (against-background [(f 1) => 1] "
                          "        (g 3 2 1) => 8 "
                          "        (h 1 2) => 7)) "
                          "... use this:"
                          "    (fact "
                          "      (g 3 2 1) => 8 "
                          "      (h 1 2) => 7 "
                          "      (against-background (f 1) => 1)) "))))
