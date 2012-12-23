(ns ^{:doc "Running tasks after a delay."}
  midje.util.scheduling
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit]))


(def scheduled-futures (atom {}))

(defn schedule [service-tag function interval]
  (let [executor (ScheduledThreadPoolExecutor. 1)
        future (.scheduleWithFixedDelay executor function 0 interval TimeUnit/MILLISECONDS)]
    (swap! scheduled-futures assoc service-tag future)))

(defn stop [service-tag]
  (.cancel (service-tag @scheduled-futures) true)
  (swap! scheduled-futures dissoc service-tag))

