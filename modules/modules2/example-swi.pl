/*
	This example shows how to expand both head and body of clauses
	using term_expand/2 and goal_expand/3.
	See post in forum:
		https://swi-prolog.discourse.group/t/modules-design-for-multi-agent-modeling/557

	So, user writes:

	far_away :-
		percepts(P),	
		member(loc(Loc), P), 
		Loc > 100.
	farther_than(D) :-
		^percepts(P),	
		member(loc(Loc), P), 
		Loc > D.


	... and it gets transformed into (percept/1 is outside the module):

	far_away(A) :-
		call(A:percepts(B)),
		member(loc(C), B),
		C>100.
	farther_than(D, A) :-
		call(A:percepts(B)),
		member(loc(C), B),
		C>D.
	
*/

% Operator used to signal that expansion is needed for the goal
:- op(1, fx, ^).

expand_clause((Head0 :- Body0), (Head :- Body)) :-
     expand_head(Head0, Head, ExtraArg),
     b_setval(extra_arg, ExtraArg),	% remember the created extra variable when processing the Body
     expand_goal(Body0, Body),	% will use defined goal_expansion/2
     b_setval(extra_arg, []).

% What heads to expand (add one more argument)
expand_head(far_away, far_away(M), M).
expand_head(farther_than(D), farther_than(D, M), M).

% What goals in bodies to change context module (use one stored in global var extra_arg
goal_expansion(^G, call(M:G)) :-
	b_getval(extra_arg, M).
goal_expansion(percepts(P), call(M:percepts(P))) :-
	b_getval(extra_arg, M).


% Domain-independent: this will be the trigger, one per clause in the DB
term_expansion(ClauseIn, ClauseOut) :-
    expand_clause(ClauseIn, ClauseOut).

% These are the clauses that will be transformed/expanded automatically
far_away :-
	percepts(P),	
	member(loc(Loc), P), 
	Loc > 100.
farther_than(D) :-
	^percepts(P),	
	member(loc(Loc), P), 
	Loc > D.
	
	
	
