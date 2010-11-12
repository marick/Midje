(ns midje.unprocessed.background)

(defn background-fakes [] (:midje/background-fakes (meta *ns*)))
(defn set-background-fakes [newval] 
  (alter-meta! *ns* merge {:midje/background-fakes newval}))

(defn push-background-fakes [fakes]
  (set-background-fakes (cons fakes (background-fakes))))

(defn pop-background-fakes [] 
  (set-background-fakes (rest (background-fakes))))

(defn background-fakes-plus [fakes]
  (flatten (cons fakes (background-fakes))))

