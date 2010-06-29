About Midje
=======================

Midje is (will be) a full-featured mocking package for
Clojure that supports three levels of syntactic sugar:

* **Unprocessed** None at all. You work with maps. For the
  person who hates my style of sugaring and wants to write
  her own.

* **Semi-sweet** When people write documentation containing
  examples, they often use a clear marker between the code
  you type and what you should see. It's often an
  arrow. Sometimes the distinction is made by making the
  example look like it's being typed at the REPL. The
  semi-sweet style adds a few constructs to make Midje tests
  look like that, while still being easy enough to generate
  from a more elaborate syntax.

* **Sweet** The style I like to use. A "little language" for
  top-down test-driven development, which repaints Freeman
  and Pryce's [Growing Object-Oriented Software, Guided by
  Tests](http://www.growing-object-oriented-software.com/)
  to the functional landscape. 

# Sweet style #

This isn't implemented yet. It will look something like this
[workthrough](http://www.exampler.com/blog/2010/06/10/tdd-in-clojure-a-sketch-part-1/)

# Semi-sweet style #

Briefly, the syntax looks like this:

    (expect (function-under-test) => 33
       (fake (mocked-function) => 33))

A [separate page](http://gist.github.com/457829) contains complete descriptions of the style 
in the form of executable examples.

# Unprocessed style #

Both calls with their expected results and expectations for
calls of faked functions are defined by maps. You can find
the format by looking at the definitions of code(fake) and
code(call-being-tested) in code(midje/semi_sweet.clj). The
function that does the actual work is code(expect*). Its
first argument is the map in the code(call-being-tested) format,
followed by zero or more maps in the code(fake) format.

Note: Midje uses the code(report) function that clojure.test
supplies. However, it uses its own type codes, so you can't
easily plug in clojure.test.tap or clojure.test.junit.

