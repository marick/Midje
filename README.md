Available via [clojars](http://clojars.org/search?q=midje)   
Current stable version: [midje "1.4.0"]        
Development version: [midje "1.5-RC1"]  - see [ the change log](https://github.com/marick/Midje/wiki/What%27s-new-in-midje-1.5)

[![Build Status](https://travis-ci.org/marick/Midje.png?branch=master)](https://travis-ci.org/marick/Midje)

About Midje
=======================

Midje is a test framework for Clojure. I created it to allow
programmers to [test with
ease](http://exampler.com/ease-and-joy.html), 
to provide a [smooth migration path from
clojure.test](https://github.com/marick/Midje/wiki/A-tutorial-introduction-for-Clojure.test-users),
to support [top-down](https://github.com/marick/Midje/wiki/The-idea-behind-top-down-development) as well as bottom-up testing,
to encourage readable tests,
to support a balance between [abstraction and
concreteness](https://github.com/marick/Midje/wiki/Metaconstants),
and to be gracious in its treatment of the people who use it.

Gallery
=======================

To make tests more readable, Midje makes them look like
examples in Clojure books. Here's how  *[The Joy of
Clojure](http://www.amazon.com/The-Joy-Clojure-Thinking-Way/dp/1935182641)*
uses syntax to explain Clojure:

![](https://raw.github.com/marick/midje-clojure-test-tutorial/master/images/other/truthy.jpg)

Here's the same fact about Clojure in Midje, expressed in a
way that's just as readable but also machine-checkable:

![](https://raw.github.com/marick/midje-clojure-test-tutorial/master/images/other/truthy-fact.jpg)

------------

While Midje assumes you'll be building a test suite with
lasting value, it allows you to do that while still using
the repl in an idiomatic way. 

Here's the start of a typical Midje repl session:

![](https://raw.github.com/marick/midje-clojure-test-tutorial/master/images/plain/3.jpg)

Midje has loaded the tests and run them for the first time.
It's also watching for file changes. When it sees them, it
loads the changed files and any dependencies. But notice
that you still have a repl prompt. That means you can move
rapidly and smoothly among  typing at the repl to try out code samples or look up
documentation, having your editor send code snippets to the
repl to evaluate, *and* saving source or test files to
immediately see what passes or fails:

![](https://raw.github.com/marick/midje-clojure-test-tutorial/master/images/plain/5.jpg)

-------------

Midje supplies prepackaged
[checkers](https://github.com/marick/Midje/wiki/Checkers)
that save you the trouble of writing common code:

![](https://raw.github.com/marick/midje-clojure-test-tutorial/master/images/other/checkers.jpg)

And so on.

Learning Midje
=======================

[Tutorial](https://github.com/marick/Midje/wiki/A-tutorial-introduction)      
[Tutorial (and sales pitch) for clojure.test user](https://github.com/marick/Midje/wiki/A-tutorial-introduction-for-Clojure.test-users)    
[User guide](https://github.com/marick/Midje/wiki)    
[Mailing list](http://groups.google.com/group/midje)

Contributors
==============
* Sean T. Allen
* Alex Baranosky
* Phillip Calçado
* Stuart Halloway
* Wilkes Joiner
* Ben Mabey
* Alan Malloy
* Brian Marick
* Bob Martin
* Paudi Moriarty
* Dmitri Naumov
* Dave Ray
* Sébastien RoccaSerra
* Harri Salokorpi
* Greg Spurrier
* Giorgio Valoti
* Joseph Wilk
