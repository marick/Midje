# Change Log
This project adheres to [Semantic Versioning](http://semver.org/).
See [here](http://keepachangelog.com/) for the change log format.

## [1.9.2] - 2018-07-17
See details in alpha releases of `1.9.2`.

## [1.9.2-alpha4] - 2018-06-22
- 332: Fix issue with `provided` in presence of laziness

## [1.9.2-alpha3] - 2018-02-21
- 436: Show exceptions raised by checker functions

## [1.9.2-alpha2] - 2018-01-03
- 431: __POTENTIALLY BREAKING__ Fix issue where you can fake functions with incorrect arities. Function faking now produces a test failure if you provide more or less arguments than expected by the function definition. This may break tests that incorrectly faked functions, but this is a good thing because it will reveal bugs in your tests.

## [1.9.2-alpha1] - 2017-12-27
- Print records in conventional way `#user.YourRecord {:arg-one 100}`, instead of `{:arg-one 100}::YourRecord`
- Fix upstream reflection warnings in `suchwow`
- Remove `clojure-commons` and `swiss-arrows` dependencies

## [1.9.1] - 2017-12-19
- pin specter version to fix import warning in suchwow
- use released version of clojure 1.9

## [1.9.0] - 2017-11-21

### Changed
- __POTENTIALLY BREAKING__ Fail when `=contains=>` targets a non-map value, such as a vector, which has unclear semantics. A fact that looks like

```clojure
(unfinished gen-list)
(fact
  (first (gen-list)) => 'list
  (provided
    (gen-list) => ..some-list..
    ..some-list.. =contains=> ['list 'contains 'not-suppored]`))
```

can be updated to

```clojure
(let [some-list ['list 'contains 'not-suppored]
  (fact
    (first (gen-list)) => 'list
    (provided
      (gen-list) => some-list)))
```

Note that clojure doesn't allow var names that begin with `.` (like `..some-metaconstant..`) in `let`-forms

### Added
- Experimental [`for-all`](https://github.com/marick/Midje/wiki/Generative-testing-with-for-all) construct for quick-check style testing, powered by `clojure.test.check`. Currently in the `midje.experimental` namespace.
- Pretty printing output and exceptions. Can be disabled by setting the `:pretty-print` configuration to `false`.
- Support for the using the same metaconstant in a function fake and `=contains=>` ([#159](https://github.com/marick/Midje/issues/159))

### Removed
- Drop support for Clojure `1.5` and `1.6`

### Fixed
See fixes noted in versions since `1.8.3`

## [1.9.0-alpha12] - 2017-11-14
- allow `for-all` to appear inside of `fact` forms

## [1.9.0-alpha11] - 2017-11-09
- [`for-all`](https://github.com/marick/Midje/wiki/Generative-testing-with-for-all) construct for quick-check style testing, powered by `clojure.test.check`
- show more info in load-time exception
- fix stacktrace printing after output colorization changes in `1.9.0-alpha10`
- add `:pretty-print` config to allow for disabling the exception and datastructure pretty-printing. `:pretty-print` is enabled by default.
- fix NPE to allow facts to be loaded via `load-string`
- re-order midje/clojure.test output to clarify which is which
- fix `null` line-number in failures occurring in `against-background`
- fix some reflection warnings

## [1.9.0-alpha10] - 2017-09-06
- Colorize and pretty print output when an actual value doesn't match an expected value

## [1.9.0-alpha9] - 2017-08-03
- Fail when `=contains=>` targets a non-map value, which has unclear semantics

## [1.9.0-alpha8] - 2017-07-04
Fixed issues:
- 389: Midje throws exception when using prerequisites for stubbing function with specific arguments
- 379: Fact running twice
- 370: Improvement on "migrating from clojure.test" tutorial
- 362: Keyword can not be used as function in `provided` clause
- 348: `just` checker is accepting incorrect maps
- 326: Wiki lacks documentation on provided
- 317: Checking function is called twice
- 281: Midje crashes on syntax
- 267: clojure.walk/postwalk fails when given a metaconstant
- 265: Midje messes up comparison of sets with vectors containing the same number of elements with different types.
- 159: Metaconstants are different

## [1.9.0-alpha6] - 2016-10-26
- Prevent use of `print` in prerequisites, which causes an infinite
  loop if Midje needs to print any errors. (#347)
- Try a bit harder to help user if clojure.tools.namespace fails. (#365)
- Compatibility with Specter 0.13
- ... which means dumping support for Clojure 1.6

## [1.9.0-alpha5]
- Ugrade clojure.unify to allow compatibility with Clojure 1.9alpha11

## [1.9.0-alpha4]
- Live with `clojure.core/any?`. Thanks, Børge Svingen

## [1.9.0-alpha3]
- PR to make Midje play better with Eastwood (via Ben Sinclair).
- Now produces fewer warnings in Jaunt (via Jason Felice)

## [1.9.0-alpha2]
- Now compatible with Specter 0.11.

## [1.9.0-alpha1]
- Drop support for Clojure 1.5
- Experimental version from @dlebrero that unloads deleted namespaces.
- `(change-defaults :run-clojure-test false)` prevents clojure.test tests from being run.
- NOTE NOTE NOTE: Previous should be documented before 1.9.0 is released.
- This is the last version compatible with Specter 0.9.X

## [1.8.3]
- Bump to newer versions of dependencies

## [1.8.2]
- Drop back to an older version of commons-codec to match what compojure uses.
  Will avoid annoying Maven messages for many users.

## [1.8.1]
- Messed up version in the project.clj file.

## [1.8.0]
- no longer indirectly drags in all of clojurescript.
- improved error messages when prerequisites are passed unrealized lazyseqs.
- obscure bug fixes

(Some other non-feature, cleanup changes were lost.)



---------------------

Older changes were in HISTORY.md, and they're not worth preserving.
