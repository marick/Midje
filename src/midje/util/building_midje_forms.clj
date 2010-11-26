(ns midje.util.building-midje-forms)

(defn ?form [] (symbol (name (ns-name *ns*)) "?form")) ; this cannot be right

(defn make-background [fake]
  (concat fake '(:type :background)))

