About Midje
=======================

Midje is (will be) a full-featured mocking package for
Clojure that supports three levels of syntactic sugar:

* **Unprocessed** None at all. You work with maps. For the
  person who hates my style of sugaring and wants to write
  her own.

* **Semi-sweet** Adds a few constructs to make tests look
  more like the examples you see in tutorials and the like.

* **Sweet** The style I like to use. A "little language" for
  top-down test-driven development, which repaints Freeman
  and Pryce's [Growing Object-Oriented Software, Guided by
  Tests](http://www.growing-object-oriented-software.com/)
  to the functional landscape. 

# Sweet style #

This isn't implemented yet. It will look something like this
[workthrough]http://www.exampler.com/blog/2010/06/10/tdd-in-clojure-a-sketch-part-1/)

# Semi-sweet style #

Here's a typical test:

(deftest some-mock-test 






# What's common to each style #

All use the clojure.test reporting mechanism, so you can
wrap your tests in code(deftest). At the moment, you're
stuck with the default test supporting--no fancy XML output,
etc. 

The semi-sweet and sweet styles are build on this
representation:

	(function arg1 arg2) => result

It's common for the values to be literals, like this:

     (function 1 2) => 4

When describing expected results of a function, though, you
can provide a function:

    (function 1 2) => even?

That means "the result of code(function 1 2) will be an even
number. Specifically, the function code(even?) is called
with the actual result as its only argument. 

What if your function-under-test is a higher-order function
that returns the code(even?) function? In that (less common)
case, you write this:

      (function 1 2) => (exactly even?) 

When describing a function call that should happen, you can
use the same syntax. So, for example, the following say that
code(subfunction) will be called with value code(1) and
return 33.

		  (subfunction 1) => 33

If you want the subfunction to return 33 for any odd argument,
use this:

    (subfunction odd?) => 33

If you want the subfunction to return 33 when it is given
exactly the function named by code(odd?), do this:

	(subfunction (exactly odd?) => 33)

There are a variety of functions that will match more than one
value. You can see all of them with this:

       (ns-publics (find-ns 'midje.checkers))

       
