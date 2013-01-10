(ns ^{:doc "An emission map whose functions can be mocked."}
  midje.emission.plugins.test-support)

(def recorder (atom :uninitialized))
(defn recorded [] @recorder)
(defn reset-recorder! []
  (reset! recorder []))

(defn record [cmd]
  (fn [& args]
    (swap! recorder #(conj % (cons cmd args)))))

(defn recording-map [& keys]
  (zipmap keys
          (map record keys)))

(def emission-map (recording-map :pass
                                 :fail
                                 :starting-to-check-fact
                                 :possible-new-namespace
                                 :forget-everything))
