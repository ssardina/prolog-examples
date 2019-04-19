:- module(withself, [with_self/2, call_self/1, percepts/1]).

% The `no_lco_ is needed to avoid last call optimization while calling Goal. 
% We pass this Module to avoid it being garbage collected.
with_self(Module, Goal) :-
     Goal,	% this will call
     no_lco(Module).

no_lco(_).

call_self(Goal) :-
    prolog_current_frame(F),
    prolog_frame_attribute(F, parent_goal, withself:with_self(Module, _)),	% who called with_self/2?
    call(Module:Goal).

%% Below we need to define hooks for every predicate in the original module that needs to recover its module
%% by tracing back the call to with_self/2 from that caller module
percepts(P) :-
    call_self(percepts(P)).
