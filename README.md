Available via [clojars](http://clojars.org/search?q=midje)   
Current stable version: [midje "1.2.0"]    
Development version: HEAD

About Midje
=======================

Midje is a test framework for Clojure. I created it to
support [top-down](https://github.com/marick/Midje/wiki/Top-down-testing) as well as bottom-up testing, to encourage
readable tests, to provide a [smooth migration path from
clojure.test](https://github.com/marick/Midje/wiki/Migrating-from-clojure.test),
to support a balance between [abstraction and
concreteness](https://github.com/marick/Midje/wiki/Metaconstants),
and to be [gracious](https://github.com/marick/Midje/wiki/Error-message-improvements) in its treatment of you, my valued guest.

Here's a simple Midje test:

      (fact
         (numerical-reverser 103) => 301)
    
I call this a "fact" because, in a world without mutability,
it just *is* a fact that the `numerical-reverser` of 103 is
301. Since I work [test-first](http://en.wikipedia.org/wiki/Test-driven_development), I like to think of programming as making false claims
about the world of the program, then changing that world to
make the claims true.

I use the arrow because I think of tests as
[examples](http://www.exampler.com/old-blog/2003/08/22/#agile-testing-project-2). When
people show snippets of code as examples, they often use an
arrow to separate what you type from what you should expect
to  see.

The object on the right-hand side of the arrow can be a
function, in which case the value of the left-hand side is
passed to it. So this is also a fact:

     (fact
       (numerical-reverser 103) => odd?)

Midje comes with a selection of predefined
[checkers](https://github.com/marick/Midje/wiki/Checkers)
that are more useful for testing than `odd?` is.

Midje's other useful features are described in the [user
guide](https://github.com/marick/Midje/wiki). If
anything there is unclear, ask in the [mailing
list](http://groups.google.com/group/midje).
If you like videos of people programming, here's an
[8-minute infomercial](http://www.youtube.com/watch?v=a7YtkcIiLGI) that concentrates on transitioning from
clojure.test. 
If
you like looking straight at code, [this heavily annotated
example](http://github.com/marick/Midje/blob/master/examples/basic/test/basic/core_test.clj)
  has a bunch. If you want to run that example, you
  can download it:

1.   [Click here](http://github.com/marick/Midje/raw/master/downloads/examples.zip).
2.   In a shell, go to the directory your browser unpacked the example into. It should be named `examples`.
3.   Go to `examples/basic/`.
4.   Type "./run" or "lein test".
5.   The facts in `test/*/core_test.clj` will be checked.

You can [download
everything](http://github.com/marick/Midje/downloads) to get
more examples.


