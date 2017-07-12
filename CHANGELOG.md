# Change Log
This project adheres to [Semantic Versioning](http://semver.org/).       
See [here](http://keepachangelog.com/) for the change log format.

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
