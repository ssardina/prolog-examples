:- module(moduleA, [testA/1, 
	assertA/1,
	reportA/1]).

% totally local
module_name(moduleA).

testA(X) :- member(X, [1,2,3]).

% asserts to module A itself
assertA(X) :- assert(X).



reportA(X) :-
	test(X),
	write("Module A reports: "),
	writeln(X).