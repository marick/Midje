(ns ^{:doc "Code to be run before, after or around facts. Also, 
            prerequisites that pertain to a group of facts."} 
  midje.parsing.1-to-explicit-form.background
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
            [midje.util.unify :as unify]
            [midje.parsing.util.file-position :as position]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.2-to-lexical-maps.fakes :as fakes]
            [midje.parsing.2-to-lexical-maps.data-fakes :as data-fakes]))

(defn background-fakes []
  (namespace-values-inside-out :midje/background-fakes))


;; dissecting background forms

(defn separate-background-forms [fact-forms]
  (let [[background-forms other-forms]
          (separate recognize/form-signaling-intention-to-wrap-background-around-fact? fact-forms)
        background-changers
          (mapcat (fn [[command & args]] (arglist-undoing-nesting args))
                  background-forms)]
    [background-changers other-forms]))

(letfn [(ensure-correct-form-variable [form]
          (translate-zipper form
            (fn [loc] (symbol-named? (zip/node loc) "?form"))
            (fn [loc] (zip/replace loc (unify/?form)))))]

  (defmacro before 
    "Code to run before a given wrapping target (:facts, :contents, :checks).
  Can take an optional keyword argument :after, for any code to run afterward.
  Used with background and against-background"
    [_wrapping-target_ before-form & {:keys [after]}]
    (ensure-correct-form-variable `(try
                                     ~before-form
                                     ?form
                                     (finally ~after))))

  (defmacro after 
    "Code to run after a given wrapping target (:facts, :contents, :checks).
  Used with background and against-background"
    [_wrapping-target_ after-form]
    (ensure-correct-form-variable `(try ?form (finally ~after-form))))

  (defmacro around 
    "Code to run around a given wrapping target (:facts, :contents, :checks).
  Use the symbol '?form' to denote the code that is being wrapped around.
     
  Ex.
  (around :contents (let [a 999] 
                      ?form
                      (print a))) 
     
  Used with background and against-background"
    [_wrapping-target_ around-form]
    (ensure-correct-form-variable around-form)))

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
                  
                  (complement recognize/first-form-could-be-a-code-runner?)
                  (error-reporter (cl-format nil "~S does not look like a prerequisite or a before/after/around code runner." (first in-progress)))
                  
                  recognize/first-form-is-a-code-runner?
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
    (macroexpand-1 (map-first #(symbol "midje.parsing.1-to-explicit-form.background" (name %)) state-description))
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

(defn body-of-against-background [[_against-background_ background-forms & background-body :as form]]
  `(do ~@background-body))

(defn against-background-contents-wrappers [[_against-background_ background-forms & _]]
  (filter (wrapping/for-wrapping-target? :contents ) (background-wrappers background-forms)))

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
  

(defn assert-valid-code-runner! [runner]
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
            (assert-valid-code-runner! changer)))))
                                      


(def against-background-forms-without-enclosed-facts (atom []))

(defmacro expecting-nested-facts [form & body]
  `(try
     (swap! against-background-forms-without-enclosed-facts #(cons 'token %))
     (let [result# ~@body]
       (when-not (empty? @against-background-forms-without-enclosed-facts)
         (error/report-error ~form
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
                             "      (against-background (f 1) => 1)) "))
       result#)
     (finally
      (swap! against-background-forms-without-enclosed-facts rest))))

(defn note-fact! []
  (reset! against-background-forms-without-enclosed-facts []))
