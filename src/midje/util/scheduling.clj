(ns ^{:doc "Running tasks after a delay."}
  midje.util.scheduling
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit]))


(def scheduled-futures (atom {}))

(defn stop [service-tag]
  (if-let [future (service-tag @scheduled-futures)]
    (future-cancel future)
    (swap! scheduled-futures dissoc service-tag)))

(defn schedule [service-tag function interval]
  (when (@scheduled-futures service-tag)
    (stop service-tag))
  (let [executor (ScheduledThreadPoolExecutor. 1)
        function (bound-fn [] (function))
        future (.scheduleWithFixedDelay executor function 0 interval TimeUnit/MILLISECONDS)]
    (swap! scheduled-futures assoc service-tag future)))

