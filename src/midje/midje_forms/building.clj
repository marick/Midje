;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.building
  (:use [midje.util.wrapping :only [ensure-correct-form-variable]]
        [midje.util.file-position :only [arrow-line-number-from-form]]))

(defn make-fake [fake-body]
  (let [line-number (arrow-line-number-from-form fake-body)]
    (vary-meta
     `(midje.semi-sweet/fake ~@fake-body)
     assoc :line line-number)))
    

(defn make-background [fake]
  (concat fake '(:type :background)))

(defmacro before [wrapping-target before-form & extras ]
  (let [after-form (second extras)]
    (ensure-correct-form-variable `(try
                                    ~before-form
                                    ?form
                                    (finally ~after-form)))))

(defmacro after [wrapping-target after-form]
  (ensure-correct-form-variable `(try ?form (finally ~after-form))))

(defmacro around [wrapping-target around-form]
  (ensure-correct-form-variable around-form))


(def *metadata-counts*)

(defmacro with-fresh-generated-metadata-names [& forms]
  `(binding [*metadata-counts* (atom {})]
     ~@forms))

(defn metaconstant-for-form [inner-form]
  (let [function-symbol (first inner-form)
        swap-fn (fn [current-value function-symbol]
                  (if (current-value function-symbol)
                    (assoc current-value function-symbol
                           (inc (current-value function-symbol)))
                    (assoc current-value function-symbol 1)))
        number ((swap! *metadata-counts* swap-fn function-symbol)
                function-symbol)]
    (symbol (format "...%s-value-%s..." (name function-symbol) number))))

