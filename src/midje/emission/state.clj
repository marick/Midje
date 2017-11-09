(ns ^{:doc "The non-configuration state of the program. That is: the workings
           that are normally invisible to any user, with the exception of tests
           and people debugging."}
  midje.emission.state)

(def ^:dynamic output-counters-atom (atom :undefined))
(defn output-counters []
  (deref output-counters-atom))
(def fresh-output-counters {:midje-failures 0 :midje-passes 0})
(defn set-output-counters! [new-val]
  (swap!  output-counters-atom (constantly new-val)))

(defn reset-output-counters!  []
  (reset! output-counters-atom fresh-output-counters))

(reset-output-counters!)

(defn output-counters:midje-passes []
  (:midje-passes (output-counters)))

(defn output-counters:midje-failures []
  (:midje-failures (output-counters)))

(defn output-counters:set:midje-passes! [value]
  (set-output-counters!
    (assoc (output-counters) :midje-passes value)))

(defn output-counters:set:midje-failures! [value]
  (set-output-counters!
    (assoc (output-counters) :midje-failures value)))

(defn output-counters:inc:midje-passes! []
  (swap! output-counters-atom (partial merge-with +) {:midje-passes 1}))

(defn output-counters:inc:midje-failures! []
  (swap!  output-counters-atom (partial merge-with +) {:midje-failures 1}))

(defmacro with-isolated-output-counters [& body]
  `(let [original-value# (output-counters)]
     (try
       (reset-output-counters!)
       ~@body
     (finally
       (set-output-counters! original-value#)))))

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
