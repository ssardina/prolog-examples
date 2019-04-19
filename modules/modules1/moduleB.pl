:- module(moduleB, [testB/1, 
	testBA/1,
	assertB/1,
	reportB/1,
	reportBB/1]).

:- use_module(moduleA).


% totally local
module_name(moduleB).

% defined by this module
testB(X) :- member(X, [a,b,c]).

% brings from moduleA
testBA(X) :- testA(X).

% asserts to module B itself
%:- module_transparent(assertB/1).
:- meta_predicate assertB(:).
assertB(X) :- assert(X).


%assert_global(Module:X) :- assert(Module:X).

:- module_transparent(reportBB/1).
reportBB(X) :-
	reportB(X).
:- module_transparent(reportB/1).
reportB(X) :-
	context_module(Module),
	Module:test(X),
	module_name(M),
	format(string(Text), "Module ~s reports ~a", [M, X]),
	writeln(Text).
	
	
	