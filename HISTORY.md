= 0.7.1
* Three new checkers: map-containing, maps-containing, and only-maps-containing

= 0.7.0 (stable)
* Chatty checkers
  http://github.com/marick/Midje/wiki/Chatty-checkers
* Added a leiningen plugin to get tidy test reports.
  http://github.com/marick/Midje/wiki/Lein-midje
* Set midje.semi-sweet/*include-midje-checks* to false to
  compile tests out of production code. 
  http://github.com/marick/Midje/wiki/Production-mode
* Incompatible change: Midje no longer defines fakes for 
  you at the top level. As with any other identifier used in
  code, they must be defined before use. The unfinished macro
  is good for that.
* Functions print as their names, if they have them, in case
  of an (exactly x) failure.
* Does a better job forcing seqs in the result to fully calculate
  themselves.
* Emacs midje-mode is a little smarter about what a Clojure
  identifier is (for M-x midje-unfinished)
  
= 0.6.1
* Midje-mode is (more) compatible with clojure-test-mode.

= 0.6.0 
* Can "unfold" nested function calls in provided facts, so
  that (f (g 1)) produces two mock calls, the first of which
  returns a metaconstant that the second expects.
* If a fact fails for more than one reason, report them all.
* midje-mode.el provides a smooth workflow for those 
  who use slime/swank.

= 0.5.0
* Line numbers almost always point to the line that provoked
  the error (one of the lines with => on them).
* The in-any-order checker should work correctly.
* Expectations (x => y) can appear at any level, so they can
  be nested within lets.

= 0.4.0
* midje.sweet in workable shape.
* (not-called f) expectation added (Wilkes Joiner)
* key&value arguments can be passed to expect(), fake(), and
  not-called() to override defaults.
* (fact) and (expect) return boolean variables because
  that's more informative in REPL.
* The checker for (f) => (throws Throwable) is supported.

= 0.3.0
* Works with 1.2-RC1 (Wilkes Joiner)

= 0.2.0

* Can fake functions from other namespaces. (Wilkes Joiner)
