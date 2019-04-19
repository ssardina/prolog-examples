:- module(moduleC, [test/1]).

:- reexport(moduleA).
:- reexport(moduleB).


test(100).
test(101).
test(102).
