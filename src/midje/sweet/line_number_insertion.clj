(ns midje.sweet.line-number-insertion
  (:require [clojure.zip :as zip])
  (:use clojure.test
	[midje.semi-sweet :only [=>]]
	[midje.util.file-position :only [line-number-known]]
	midje.util.forms)
)

;; Yeah, it's not tail-recursive. So sue me.
(defn arrow-line-number [arrow-loc]
  (try (or  (-> arrow-loc zip/left zip/node meta :line)
	    (-> arrow-loc zip/right zip/node meta :line)
	    (inc (arrow-line-number (zip/prev arrow-loc))))
       (catch Throwable ex nil)))

;; Adding line numbers

(defn add-line-number-to-end-of-arrow-sequence__no-movement [number loc]
  (-> loc
      zip/right
      (zip/insert-right `(line-number-known ~number))
      (zip/insert-right :file-position)
      zip/left))

(defn add-line-numbers [form]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond (namespacey-match '(=>) loc)
			     (add-line-number-to-end-of-arrow-sequence__no-movement
			      (arrow-line-number loc)
			      loc)

			     :else loc))))))

