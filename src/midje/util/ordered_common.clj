(ns midje.util.ordered-common
  "This code was copied from an earlier version of the ordered library,
  `https://github.com/flatland/ordered`, because of a version conflict.
  That library is covered by the Eclipse Public License, V1.0, which
  you can find in Midje's root directory."
)

(defmacro change! [field f & args]
  `(set! ~field (~f ~field ~@args)))

(defprotocol Compactable
  (compact [this]))
