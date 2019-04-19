:- module(test_kb2, [loc/1]).

:- use_module(withself).
:- reexport(withself, [with_self/2]).

    
loc(X) :-
	percepts(P),
	member(loc(X), P).
	   