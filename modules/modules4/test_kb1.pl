:- module(test_kb1, [far_away/1, far_away2/1]).

:- use_module(withself).
:- reexport(withself, [with_self/2]).

:- use_module(test_kb2).
    
% Is destination far from the current agent location?
% 	Agent location LocAgent is obtained via the percept of the agent, which is not in this module
% 	but in the agent "calling" module
far_away(Destination) :-
	percepts(P),	% this should be transformed into test_agent:percept(P) automatically
	member(loc(LocAgent), P), !,
	distance(LocAgent, Destination, D),
	D > 10.

far_away2(Destination) :-
	loc(LocAgent),	% this should be transformed into test_agent:percept(P) automatically
	distance(LocAgent, Destination, D),
	D > 10.
	

distance(L1, L2, D) :- D is abs(L1-L2).
	