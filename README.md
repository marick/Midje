About Midje
=======================

Midje is a test framework for Clojure that emphasizes
mocking and [ease of use](http://exampler.com/ease-and-joy.html). It supports three levels of
syntactic sugar:

* **Unprocessed** None at all. You work with maps. For the
  person who hates my style of sugaring and wants to write
  her own.


* **Semi-sweet** Here's a simple Midje test:
       
         (expect (numerical-reverser 103) => 301)
    
    I use the arrows because I think of tests as
    [examples](http://www.exampler.com/old-blog/2003/08/22/#agile-testing-project-2). When
    people show snippets of code as examples, they often use
    an arrow to 
    separate what you type from what you should expect to
    see. The Midje line above says "After
    `numerical-reverser` is given `103`, the test should see it
    return `301`."
    
    In that form, Midje does the same thing as
    clojure.test. (Indeed, it uses clojure.test's
    reporting mechanism.) If, however, you want
    `numerical-reverser` to use a function that
    hasn't been coded yet, you can fake out that function like
    this: 

        (expect (numerical-reverser 103) => 301
           (fake (string-reverser "103") => "301"))

    In the second line, the arrow signifies that when
    `string-reverser` is given `"103"`, we know its caller
    will get back `"301"`

    The semi-sweet style does nothing more than add
    the `expect` and `fake` macros, which are ways of
    generating the maps the unprocessed style expects while themselves
    not being too hard to generate from more ambitious macros.

* **Sweet** The style I like to use. A "little language" for
  top-down test-driven development, which repaints Freeman
  and Pryce's [Growing Object-Oriented Software, Guided by
  Tests](http://www.growing-object-oriented-software.com/)
  to the functional landscape. There, I like to think of TDD
  as building a web of interrelated facts. For example,
  here's a fact about `numerical-reverser`:

         (fact 
            (numerical-reverser 103) => 301)

  But that fact is only true for this implementation
  provided something else is true for `string-reverser`:

        (fact
          (numerical-reverser 103) => 301
          (provided 
            (string-reverser "103") => "301"))


# For the impatient #

You can easily see Midje in action and fiddle with some
tests. You don't even need Clojure installed. Here's how:

* [Click here](http://github.com/marick/Midje/raw/master/downloads/examples.zip).
* In a shell, go to the directory your browser unpacked the example
  into. It should be named `examples`.
* For a sweet example, go to
  `examples/sweet-examples/basic/`. For a semi-sweet
  example, go to `examples/semi-sweet-examples`. 
* Type "./run".  (Windows users, see README.html)
* To change the tests, edit `test/*/core_test.clj`.

You can [download everything](http://github.com/marick/Midje/downloads) to get more examples.

# Documentation #

* [This heavily annotated example](http://github.com/marick/Midje/blob/master/examples/sweet-examples/basic/test/basic/core_test.clj)
  of the sweet style is the place to look. Also see the [wiki](http://github.com/marick/Midje/wiki).

* [Another example](http://github.com/marick/Midje/blob/master/examples/semi-sweet-examples/test/semi_sweet_simple/core_test.clj) contains
complete descriptions of the semi-sweet style.

* In the unprocessed style, both calls with their expected results and expectations for
calls of faked functions are defined by maps. You can find
the format by looking at the definitions of `fake` and
`call-being-tested` in `midje/semi_sweet.clj`. The
function that does the actual work is `expect*`. Its
first argument is the map in the `call-being-tested` format,
followed by zero or more maps in the `fake` format.

# Community

[The mailing list](http://groups.google.com/group/midje)

The [wiki](http://github.com/marick/Midje/wiki) is growing documentation about such things as
Emacs support and details of use.
