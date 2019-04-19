:- module(test_agent, [agent_far_from/1]).

:- use_module(test_kb).

agent_module(M) :-
	context_module(M).

% the latest percept of the agent
percepts([loc(3), charge(20), health(10)]).

% Convert every call of far_away(X) to far_away(Y, M) where M is the context module of agent.
goal_expansion(far_away(X), (agent_module(M), far_away(X, M))).


% Check if agent, from percept, is far away from D
agent_far_from(D) :-
	far_away(D).
	
