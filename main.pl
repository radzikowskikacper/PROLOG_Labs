	%main(List):-
	%goals_achieved(List, [on(b2, b7), on(b7, b4), on(b4, p1), on(b3, p3), clean(p2), clean(p4), clean(b2), clean(b3)]).
main(Goal, Goals, Rest):-
	choose_goal(Goal, Goals, Rest, [on(b2, b7), on(b7, b4), on(b4, p1), on(b3, p3), clean(p2), clean(p4), clean(b2), clean(b3)]).
 
choose_goal(Goal, [Goal | Rest], Rest, InitState) :- 
	not( member(Goal, InitState) ), !.
	
choose_goal(Goal, [ _ | Rest], NewRest, InitState) :- 
	choose_goal(Goal, Rest, NewRest, InitState).

	
	
	
	
achieves(move(X, Z, Y), on(X, Y)).

achieves(move(X, Z, Y), on(X, Y / on(Y, P))).

achieves(move(Y, X, Z), clean(X)):-
  atomic(X).

achieves(move(P, X, R), clean(X / on(X, Y))):-
  not(atomic(X)).
	
%requires(move(X, Y, Z), [clean(X), clean(Z)]):-
%  not(atomic(Y)).

%requires(move(X, Y, Z), [clean(X), clean(Z)]):-
 % atomic(Y).





	
goals_achieved([], _).

goals_achieved([G|R], State) :-
	check_goal(G, State),
	goals_achieved(R, State).

check_goal(G, State):-
	parse(G, GList),
	%writeln(GList),
	subset(GList, State).

parse(nill, []).	
parse(clean(X), [clean(X)]).
parse(on(X, Y),  [on(X, Y)]).
	
parse(clean(X/C), [clean(X) | Rest]) :-
	parse(C, Rest).

parse(on(X, Y/C),  [on(X, Y) | Rest]) :-
	parse(C, Rest).


	
plan(State, Goals, [ ], State) :-
	goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState):-
	choose_goal(Goal, Goals, RestGoals, InitState),
	achieves(Action, Goal),
	requires(Action, Conditions),
	plan(InitState, Conditions, PrePlan, MidState1),
	perform_action(MidState1, Action, MidState2),
	plan(MidState2, RestGoals, PostPlan, FinalState),
	conc(PrePlan, [ Action | PostPlan ], Plan).