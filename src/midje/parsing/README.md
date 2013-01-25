The core of midje is one function and a few different kinds
of maps. The parsing code is organized around the maps, and
the function comes along for the ride.

For reference, the maps are gathered together in
`lexical_maps.clj` in this directory. Partly for historical
reasons, parsing is a 3 step process, each step represented
by one of the `1_*`, `2_*`, and `3_*` directories. (The
`0_*` directory will be explained later.)

## Step 1: Converting from implicit to explicit form

Midje facts are built around the ideas of examples and
prerequisites. In normal Lispy style, you'd expect those
ideas to be reified into the names of functions or macros,
and you'd expect to see code forms like this:

```clojure
    (example ... )
    (prerequisite ...)
```

Midje instead uses an implicit syntax. When you see a form
like this:

```clojure
      (facts "about my-fun"
        (myfun 3) => 4
        (myfun -1) => 4
        (provided
           (helper 1) => 2
           (helper 2) => 2))
```

... you know there are two examples, the second of which
identifies two calls to a single prerequisite
(`helper`). But the words don't appear. You infer that from
a non-Lispy syntax.

This step in parsing converts those implicit forms into
explicit ones. (For historical reasons, the names don't match
current Midje terminology.) Here are the explicit forms from
the above fact:

```clojure
   ...
   (expect (myfun 3) => 4)
   (expect (myfun -1 => 4 (fake (helper 1) => 2)
                          (fake (helper 2) => 2)))
   ...
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
before it is filled with the right values. Here's 
part of an example's lexical map:

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

