(ns ^{:doc "A compendium is 'a collection of concise but detailed information
            about a particular subject. The Midje compendium contains
            the currently relevant facts about your program."}
  midje.ideas.compendium
  (:use [midje.ideas.metadata :only [separate-metadata]]
        [bultitude.core :only [namespaces-in-dir]]))

(def ^{:dynamic true} *parse-time-fact-level* 0)

(defmacro given-possible-fact-nesting [& forms]
  `(binding [*parse-time-fact-level* (inc *parse-time-fact-level*)]
     ~@forms))

(defmacro working-on-nested-facts [& forms]
  ;; Make sure we don't treat this as a top-level fact
  `(binding [*parse-time-fact-level* (+ 2 *parse-time-fact-level*)]
     ~@forms))


(def fact-check-history (atom (constantly true)))

(defn dereference-history []
  @(ns-resolve 'midje.ideas.compendium @fact-check-history))
  

(defn wrap-with-check-time-fact-recording [true-name form]
  (if (= *parse-time-fact-level* 1)
    `(do (record-fact-check '~true-name)
         ~form)
    form))

(defn- force-namespace-name [namespace-or-symbol]
  (if (= (type namespace-or-symbol) clojure.lang.Namespace)
    (ns-name namespace-or-symbol)
    namespace-or-symbol))

(def by-namespace-compendium (atom {}))

(defn forget-facts-in-namespace [namespace]
  (swap! by-namespace-compendium dissoc (force-namespace-name namespace)))
  

(defn reset-compendium []
  (reset! by-namespace-compendium {}))



(defn compendium-contents []
  (apply concat (vals @by-namespace-compendium)))
  
(defn namespace-facts [namespace]
  (get @by-namespace-compendium
       (force-namespace-name namespace)))

;; TODO: the use of the true-name symbol means accumulation of
;; non-garbage-collected crud as functions are redefined. Worry about
;; that later.

;; I must be brain-dead, because this code has got to be way too complicated.
(defn record-fact-existence [function]
  (let [{fact-namespace :midje/namespace true-name :midje/true-name midje-name :midje/name} (meta function)]
    (intern 'midje.ideas.compendium true-name function)
    (when midje-name
      (let [same-namespace-functions (namespace-facts fact-namespace)
            without-old (remove (fn [f]
                                  (= midje-name (:midje/name (meta f))))
                                same-namespace-functions)]
        (swap! by-namespace-compendium
               assoc fact-namespace without-old)))
    (swap! by-namespace-compendium
           #(merge-with concat % { fact-namespace [function] }))))

(defn record-fact-check [true-name]
  (reset! fact-check-history true-name))

(defn check-some-facts [fact-functions]
  (every? true? (map #(%) fact-functions)))

;;; Loading facts

(defn fact-namespaces [& args]
  ;; You get an obscure error if you pass a keyword to
  ;; namespaces-in-dir. I'd rather accept all kinds of typos than
  ;; subject a user to that.
  (let [[dirs [_keyword_ prefix & junk]] (split-with string? args)
        desireds (if (empty? dirs) ["test"] dirs) 
        actuals (mapcat namespaces-in-dir desireds)]
    (filter #(.startsWith (name %) (or prefix "")) actuals)))
  
