(ns ^{:doc
"This namespace contains [will eventually contain] all the
 non-configuration state of the program. That is: the workings
 that are normally invisible to any user, with the exception
 of tests and people debugging."}
  midje.emission.state
  (:require [midje.util.ecosystem :as ecosystem]))

(def top-level-fact-output-lines [])
(def fact-output-lines [])


;;; At some point, figure out how to do nested splicing quotes.
(defn make-defmacro-body [ns name reset-name set-name body]
  `(let [original-value# (deref (deref (ns-resolve '~ns '~name)))]
     (try
       ((ns-resolve '~ns '~reset-name))
       ~@body
     (finally
       ((ns-resolve '~ns '~set-name) original-value#)))))
    

(defmacro make-counter-atom [name & keys]
  (let [fresh-name (symbol (str "fresh-" name))
        set-name (symbol (str "set-" name "!"))
        reset-name (symbol (str "reset-" name "!"))
        wrapper-name (symbol (str "with-isolated-" name))]
    (letfn [(make-one-incrementer [key]
              `(defn ~(symbol (str name ":inc" key "!")) []
                 (swap! ~name
                        (partial merge-with +) {~key 1})))
            (make-one-getter [key]
              `(defn ~(symbol (str name key)) []
                 (~key (deref ~name))))
            (make-defmacro []
              (let [body-sym (gensym 'body-)]
                `(defmacro ~wrapper-name [& ~body-sym]
                   (make-defmacro-body '~(ns-name *ns*) '~name '~reset-name '~set-name ~body-sym))))]
      `(do
         (def ~name (atom 0))
         (ecosystem/when-1-3+ (.setDynamic (var ~name)))
         (def ~fresh-name ~(zipmap keys (repeat 0)))
         (defn ~set-name [newval#] (swap! ~name (constantly newval#)))
         (defn ~reset-name [] (reset! ~name ~fresh-name))
         (~reset-name)
         ~(make-defmacro)
         ~@(map make-one-getter keys) 
         ~@(map make-one-incrementer keys)))))




(make-counter-atom output-counters
  :midje-passes :midje-failures :clojure-test-passes :clojure-test-failures)

(defonce ^{:dynamic true} emission-functions nil)

(defonce installation-ok? true)

(defn install-emission-map-wildly [map]
  (alter-var-root #'emission-functions (constantly map)))

(defn install-emission-map [map]
  (when installation-ok? (install-emission-map-wildly map)))

(defn no-more-plugin-installation []
  (alter-var-root #'installation-ok? (constantly false)))

(defmacro with-emission-map [map & body]
  `(binding [emission-functions ~map]
     ~@body))
  
