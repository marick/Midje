Midje parsing generates different kinds of maps that drive
checking. For reference, the maps are gathered together in
`lexical_maps.clj` in this directory. Partly for historical
reasons, parsing is a 3 step process, each step represented
by one of the `1_*`, `2_*`, and `3_*` directories. (The
`0_*` directory will be explained later.

## Step 1: Converting from unclojurian to more clojurian syntax.

Midje is notable (and, in some circles, notably disliked)
for having a syntax that's not nicely nested in the normal
Lispy way. For example, individual examples are not enclosed by
parens:

```clojure
      (fact
         (+ 1 1) => 2      ; The arrow is significant for parsing.
         (+ 2 2) => 4)
```

Files in `1_to_normal_form` convert from Midje syntax to a
more Lispy syntax. Specifically, examples are converted into
a form like this:

```clojure
    (expect (+ 1 1) => 2)
```

While still a macro, `expect` looks more like Lisp.
(The arrow is not just decoration, since different forms of
arrows affect what kind of check is done.)

Prerequisites are inserted as additional clauses within the arrow
form. So this example:

```clojure
    (fact
      (f 1) => 2
      (provided
         (g 1) => 3))
```

... becomes this:

```clojure
    (expect (f 1) => 2
                (fake (g 1) => 3))
```

## Step 2: Converting into lexical maps

The next step is driven by the need to obey lexical
scoping. Consider the following:

```clojure
    (let [a 1]
      (fact (* a 1) => a))
```

... which expands to this:

```clojure
    (expect (* a 1) => a)
```

At the time the `expect` is evaluated, we don't know the
value of `a`. (Note: the core midje code was written before
&env was available - I think - and I'm still not sure if
&env has a published API that we could use.) Therefore,
expect expands out into what I'm calling a "lexical map" - a
form that needs to be evaluated in the lexical context
before it is filled with the right values. Here's an example
of part of an example's lexical map:

```clojure
    {:function-under-test (fn [] (* a 1))
     :expected-result a
     :expected-result-form 'a}
```

So that there's a single point of reference, the lexical
maps are stored in `parsing/lexical_maps.clj`.

## Step 3: Code executes to make the lexical map an evaluated map

The lexical map shown above uses only core Clojure. Other
maps might not. For example, a `fake` map uses Midje
functions to generate matching functions:

```clojure
    {:arg-matchers (map mkfn:arg-matcher [1 (roughly 2)])
     :result-supplier (mkfn:result-supplier lexical-var)}
```

Such functions are defined in the `3_*` directory.


## Forms that build on facts

As I write, Midje has two macros, `tabular` and `formula`,
that expand into code that wrap facts. Such macros are
defined in the `0_*` directory.

