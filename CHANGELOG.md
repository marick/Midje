# Change Log
This project adheres to [Semantic Versioning](http://semver.org/).       
See [here](http://keepachangelog.com/) for the change log format.

## [unreleased]

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
