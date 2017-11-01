(ns ^{:doc "The non-configuration state of the program. That is: the workings
           that are normally invisible to any user, with the exception of tests
           and people debugging."}
  midje.emission.state)

;;; At some point, figure out how to do nested splicing quotes.
(defn make-defmacro-body [ns name reset-name set-name body]
  `(let [original-value# ((ns-resolve '~ns '~name))]
     (try
       ((ns-resolve '~ns '~reset-name))
       ~@body
     (finally
       ((ns-resolve '~ns '~set-name) original-value#)))))

(defmacro make-counter-atom [name & keys]
  (let [atom-name (symbol (str name "-atom"))
        fresh-name (symbol (str "fresh-" name))
        set-name (symbol (str "set-" name "!"))
        reset-name (symbol (str "reset-" name "!"))
        wrapper-name (symbol (str "with-isolated-" name))]
    (letfn [(make-one-incrementer [key]
              `(defn ~(symbol (str name ":inc" key "!")) []
                 (swap! ~atom-name
                        (partial merge-with +) {~key 1})))
            (make-one-getter [key]
              `(defn ~(symbol (str name key)) []
                 (~key (~name))))
            (make-one-setter [key]
              `(defn ~(symbol (str name ":set" key "!")) [value#]
                 (~set-name (assoc (~name) ~key value#))))
            (make-defmacro []
              (let [body-sym (gensym 'body-)]
                `(defmacro ~wrapper-name [& ~body-sym]
                   (make-defmacro-body '~(ns-name *ns*) '~name '~reset-name '~set-name ~body-sym))))]
      `(do
         (def ~atom-name (atom :undefined))
         (defn ~name [] (deref ~atom-name))
         (.setDynamic (var ~atom-name))
         (def ~fresh-name ~(zipmap keys (repeat 0)))
         (defn ~set-name [newval#] (swap! ~atom-name (constantly newval#)))
         (defn ~reset-name [] (reset! ~atom-name ~fresh-name))
         (~reset-name)
         ~(make-defmacro)
         ~@(map make-one-getter keys)
         ~@(map make-one-setter keys)
         ~@(map make-one-incrementer keys)))))

(make-counter-atom output-counters
  :midje-passes :midje-failures)

(def raw-fact-failures-atom (atom :uninitialized))
(def raw-fact-failures #(deref raw-fact-failures-atom))
(def forget-raw-fact-failures! #(reset! raw-fact-failures-atom []))
(forget-raw-fact-failures!)
(def add-raw-fact-failure! (partial swap! raw-fact-failures-atom conj))

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
