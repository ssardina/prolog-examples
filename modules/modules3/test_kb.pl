:- module(test_kb, [far_away/1, with_self/2]).

% The `no_lco_ is needed to avoid last call optimization while calling Goal. 
% We pass this Module to avoid it being garbage collected.
with_self(Module, Goal) :-
     Goal,	% this will call
     no_lco(Module).

no_lco(_).

call_self(Goal) :-
    prolog_current_frame(F),
    prolog_frame_attribute(F, parent_goal, test_kb:with_self(Module, _)),	% who called with_self/2?
    call(Module:Goal).


percepts(P) :-
    call_self(percepts(P)).

    
% Is destination far from the current agent location?
% 	Agent location LocAgent is obtained via the percept of the agent, which is not in this module
% 	but in the agent "calling" module
far_away(Destination) :-
	percepts(P),	% this should be transformed into test_agent:percept(P) automatically
	member(loc(LocAgent), P), !,
	distance(LocAgent, Destination, D),
	D > 10.


distance(L1, L2, D) :- D is abs(L1-L2).
	