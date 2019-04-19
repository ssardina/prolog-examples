:- module(test_agent, [agent_far_from/1, agent_far_from2/1]).

:- use_module(test_kb1, [with_self/2]).


% the latest percept of the agent
percepts([loc(3), charge(20), health(10)]).


% Check if agent, from percept, is far away from D
agent_far_from(D) :-
	far_away(D).
agent_far_from2(D) :-
	far_away2(D).

far_away(X) :-
     context_module(Self),
     with_self(Self, test_kb1:far_away(X)).	% this is defined in test_kb
     
     
far_away2(X) :-
     context_module(Self),
     with_self(Self, test_kb1:far_away2(X)).	% this is defined in test_kb