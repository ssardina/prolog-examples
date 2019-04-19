/**
	Changing the context of a goal.
	See https://swi-prolog.discourse.group/t/modules-design-for-multi-agent-modeling/557/11


	Example to convert:

	far_away(Destination) :-
		percepts(P),
		member(loc(LocAgent), P), !,
		distance(LocAgent, Destination, D),
		D > 10.

	will be converted into (where A is the module context of caller)

	far_away(D, A) :-
		call(A:percepts(B)),
		member(loc(C), B), !,
		distance(C, D, E),
		E>10.

	It will also convert any goal of the form ^G
*/
:- module(test_kb, [far_away/2]).

% operator to signal when the predicate should be expanded with context module call
%	Every goal of the form ^G will be expanded to M:G (where M is the added last argument in the head of clause)
:- op(1, fx, ^).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPAND CLAUSES
%
% Expand all clauses, by adding an argument variable to the head and 
% to some of the predicates in the body as stated by expand_head/2 below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_clause((Head0 :- Body0), (Head :- Body)) :-
     expand_head(Head0, Head, ExtraArg),
     b_setval(extra_arg, ExtraArg),	% remember the created extra variable when processing the Body
     expand_goal(Body0, Body),	% will use defined goal_expansion/2
     b_setval(extra_arg, []).


% if a body contains far_away(X) it will be expanded to far_away(X, Y)
expand_head(far_away(X), far_away(X, M), M) :- !.
expand_head(Head0, Head, ExtraArg) :-	
	fail,	% do not use this rule or it will expand every clause in the DB
	Head0 =.. [F|L],
	append(L, [ExtraArg], L2),
	Head =.. [F|L2].

% What goals in bodies need to have another context module (the one stored in extra_arg)
goal_expansion(^G, call(M:G)) :-
	b_getval(extra_arg, M).
goal_expansion(percepts(P), call(M:percepts(P))) :-
	b_getval(extra_arg, M).


% Domain-independent: this will be the trigger, one per clause in the DB
term_expansion(ClauseIn, ClauseOut) :-
    expand_clause(ClauseIn, ClauseOut).

    
% Is destination far from the current agent location?
% 	Agent location LocAgent is obtained via the percept of the agent, which is not in this module
% 	but in the agent "calling" module
far_away(Destination) :-
	percepts(P),
	member(loc(LocAgent), P), !,
	distance(LocAgent, Destination, D),
	D > 20.
far_away(Destination) :-
	^percepts(P),
	member(loc(LocAgent), P), !,
	distance(LocAgent, Destination, D),
	D > 20.
	
distance(L1, L2, D) :- D is abs(L1-L2).	
