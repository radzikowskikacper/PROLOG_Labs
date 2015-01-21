	%main(List):-
	%goals_achieved(List, [on(b2, b7), on(b7, b4), on(b4, p1), on(b3, p3), clean(p2), clean(p4), clean(b2), clean(b3)]).
main(Goal, Goals, Rest):-
	choose_goal(Goal, Goals, Rest, [on(b2, b7), on(b7, b4), on(b4, p1), on(b3, p3), clean(p2), clean(p4), clean(b2), clean(b3)]).
 
choose_goal(Goal, [Goal | Rest], Rest, InitState) :- 
	not( member(Goal, InitState) ), !.
	
choose_goal(Goal, [ _ | Rest], NewRest, InitState) :- 
	choose_goal(Goal, Rest, NewRest, InitState).

	
	
	
	
achieves(move(X, _, Y), on(X, Y)).

achieves(move(X, _, Y), on(X, Y / on(Y, _))).

achieves(move(_, X, _), clean(X)):-
  atomic(X).

achieves(move(_, X, _), clean(X / on(X, _))):-
  not(atomic(X)).

  
requires(move(X, _, Z), [clean(X), clean(Z)], _):-
  atomic(X),!.

requires(move(X, Y, Z), [clean(X / on(X, Y), clean(Z / nil))], _):-
  atomic(Y).

requires(move(X, Y, Z), [clean(X / on(X, Y / on(Y, Target))), clean(Z / nil)], Target):-
  not(atomic(Y)).




	
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

gen(Max, Max1):-
	current_predicate(tempmax/1),!,
	tempmax(Max2),
	Max1 is Max2 + 1,
	Max1 < Max,
	retract(tempmax(Max2)),
	assert(tempmax(Max1)).

gen(_, 1):-
	assert(tempmax(1)).
	
plan(State, Goals, [ ], State, _, _, _) :-
	goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState, Max, Ctr, EndCtr):-
	Ctr < Max,
	Ctr1 is Ctr + 1,
	gen(Max, Max1),
	
	choose_goal(Goal, Goals, RestGoals, InitState),
	achieves(Action, Goal),
	requires(Action, Conditions),
	
	plan(InitState, Conditions, PrePlan, MidState1, Max1, Ctr1, Ectr1),
	perform_action(MidState1, Action, MidState2),
	Max2 is Max - Ectr1,
	
	plan(MidState2, RestGoals, PostPlan, FinalState, Max2, Ctr1, _),
	conc(PrePlan, [ Action | PostPlan ], Plan).