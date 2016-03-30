(ns midje.util.ordered-deftype
  "This code was copied from an earlier version of the ordered library,
  `https://github.com/flatland/ordered`, because of a version conflict.
  That library is covered by the Eclipse Public License, V1.0, which
  you can find in Midje's root directory."
)

(letfn [;; given a mess of deftype specs, possibly with classes/interfaces
        ;; specified multiple times, collapse it into a map like
        ;; {interface => (method1 method2...)}.
        ;; needed because core.deftype only allows specifying a class ONCE,
        ;; so our delegating versions would clash with client's custom methods.
        (aggregate [decls]
          (loop [ret {}, curr-key nil, decls decls]
            (if-let [[x & xs] (seq decls)]
              (if (seq? x)
                (recur (update-in ret [curr-key] conj x),
                       curr-key, xs)
                (recur (update-in ret [x] #(or % ())),
                       x, xs))
              ret)))

        ;; Given a map returned by aggregate, spit out a flattened deftype body
        (explode [aggregated]
          (apply concat
                 (for [[k v] aggregated]
                   (cons k v))))

        ;; Output the method body for a delegating implementation
        (delegating-method [method-name args delegate]
          `(~method-name [~'_ ~@args]
             (. ~delegate (~method-name ~@args))))

        ;; Create a series of Interface (method...) (method...) expressions,
        ;; suitable for creating the entire body of a deftype or reify.
        (type-body [delegate-map other-args]
          (let [our-stuff (for [[send-to interfaces] delegate-map
                                [interface which] interfaces
                                :let [send-to (vary-meta send-to
                                                         assoc :tag interface)]
                                [name args] which]
                            [interface (delegating-method name args send-to)])]
            (explode (aggregate (apply concat other-args our-stuff)))))]

  (defmacro delegating-deftype
    "Shorthand for defining a new type with deftype, which delegates the methods
you name to some other object or objects. Delegates are usually a member field,
but can be any expression: the expression will be evaluated every time a method
is delegated. The delegate object (or expression) will be type-hinted with the
type of the interface being delegated.

The delegate-map argument should be structured like:
{object-to-delegate-to {Interface1 [(method1 [])
                                    (method2 [foo bar baz])]
                        Interface2 [(otherMethod [other])]},
 another-object {Interface1 [(method3 [whatever])]}}.

This will cause your deftype to include an implementation of Interface1.method1
which does its work by forwarding to (.method1 object-to-delegate-to), and
likewise for the other methods. Arguments will be forwarded on untouched, and
you should not include a `this` parameter. Note especially that you can have
methods from Interface1 implemented by delegating to multiple objects if you
choose, and can also include custom implementations for the remaining methods of
Interface1 if you have no suitable delegate.

Arguments after `delegate-map` are as with deftype, although if deftype ever has
options defined for it, delegating-deftype may break with them."
    [cname [& fields] delegate-map & deftype-args]
    `(deftype ~cname [~@fields]
       ~@(type-body delegate-map deftype-args)))
  (defmacro delegating-reify
    "Like delegating-deftype, but creates a reify body instead of a deftype."
    [delegate-map & reify-args]
    `(reify ~@(type-body delegate-map reify-args))))
