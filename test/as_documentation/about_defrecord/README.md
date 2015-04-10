Protocols complicate testing when you want to use protocol functions
as prerequisites. There are two issues:

1. Because protocol functions are inlined, they can't normally be used
    in a `provided` clause. If you want to do that, you have to use a
    special-to-Midje macro. Such a definition is safe to use in
    production code, but at the expense of a function call and test.

2. Even if you use `defrecord-openly`, you may well stumble over
    some fine points of how protocols work - fine points you wouldn't
    need to know in normal use. This set of files aims to explain them.

All files build off a single protocol definition in `as-documentation.about-defrecord.generic`. Read that first. Then read the `specific.clj` and `test.clj` files (in that order) in each of these directories (in this order):

1. using_as__plain_tests
2. using_refer__plain_tests
2. using_as__provided_tests
4. using_refer__provided_tests
