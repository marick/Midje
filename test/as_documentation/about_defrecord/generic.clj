(ns as-documentation.about-defrecord.generic)

;; Here's a simple protocol. To cover various cases, there's two arities for `bump` and 
;; `twice` begs out to use `bump` in its implementation.
;;
;; This namespace is named `generic` because the functions declared by records/types
;; that implement this protocol are akin to what CLOS calls "generic functions", which
;; are specialized according to the types of the arguments (in this case, only according
;; to the first, `this`, argument).
;; See http://en.wikipedia.org/wiki/Generic_function

(defprotocol Numerical
  "A record for some sort of number-holder, with operations on that number."
  (bump [this] [this by]
    "Return the value of the enclosed number bumped by either the given `by` or the unit")
  (twice [this]
    "Extract twice the enclosed numerical value, where \"twice\" is defined by the implementation"))
