%main(List):-
	%goals_achieved(List, [on(b2, b7), on(b7, b4), on(b4, p1), on(b3, p3), clear(p2), clear(p4), clear(b2), clear(b3)]).

%main(Goal, Goals, Rest):-
	%choose_goal(Goal, Goals, Rest, [on(b2, b7), on(b7, b4), on(b4, p1), on(b3, p3), clear(p2), clear(p4), clear(b2), clear(b3)]).

%main(Action, State):-
%	perform_action([on(b1, p1), clear(p2), clear(p3), clear(p4), clear(b1)], Action, State).

main(Goals, Plan, FinalState):-
	plan([on( b4, p1), on( b1, b4),
	on(b3, b1), on(b2, p3),
	clear(b3), clear(b2),
	clear(p2), clear(p4) ],
	Goals, 
	Plan, 
	FinalState, 0).
	
main2(Goals, Plan, FinalState):-
	plan([on( b5, b2), on( b4, p1), on( b1, b4),
	on(b3, b1), on(b2, p3), 
	clear(b5),
	clear(b3),
	clear(p2), clear(p4) ],
	Goals, 
	Plan, 
	FinalState, 0).
	
% niedeterministyczny  wybór celu 

choose_goal(Goal, [Goal | Rest], Rest, InitState) :- 
	not( member(Goal, InitState) ).
	
choose_goal(Goal, [ PrevGoal | Rest ], [ PrevGoal | NewRest ], InitState) :- 
	choose_goal(Goal, Rest, NewRest, InitState).
	
% END niedeterministyczny wybór celu	
	

achieves(move(X, _, Y), on(X, Y), _).

achieves(move(X, _, Y), on(X, Y / on(Y, _)), _).

achieves(move(_, X, _), clear(X), _):-
  atomic(X).

achieves(move(_, X, _), clear(X / on(X, Y)), Y):-
  not(atomic(X)).

  
requires(move(X, A, Z), [clear(X), clear(Z), on(X, A)], _):-
  atomic(X), atomic(Z), !.

requires(move(X, Y, Z), [clear(X / on(X, Y)), clear(Z / dist(X,Z))], _):-
  atomic(Y).

requires(move(X, Y, Z), [clear(X / on(X, Y / on(Y, Target))), clear(Z / dist(X,Z))], Target):-
  not(atomic(Y)).

goals_achieved(Goals, State) :-
	check_goal(Goals, NewGoals, Distinct),
	my_subset(NewGoals, State),
	distinct_pairs(Distinct),
    writeln('goals_achieved - ok').
	
check_goal([], [], []):-!.
check_goal([G|Rest], NewGoals, Distinct):-
	parse(G, GList, PartDis),
	check_goal(Rest, RestNewGoals, RestDis),
	append(PartDis, RestDis, Distinct),
	append(GList, RestNewGoals, NewGoals).
	
	 

parse(dist(A,B), [], [dist(A,B)]).

parse(clear(X/C), [clear(X) | Rest], Distinct) :-
  nonvar(C),!,
  parse(C, Rest, Distinct).

parse(clear(X), [clear(X)], []).

parse(on(X, Y/C),  [on(X, Y) | Rest], []) :-
  nonvar(C),!,
  parse(C, Rest, []).

parse(on(X, Y),  [on(X, Y)], []).


distinct_pairs([]).
distinct_pairs([dist(A,B)|Rest]):-
	A\=B,
	distinct_pairs(Rest).



my_subset([], _) :- !.
my_subset([A|GList], State):-
	get-any(B, State, SRest),
	A = B,
	my_subset(GList, SRest).


get-any(A, [RItem | List], [RItem |NewRest] ) :- get-any(A, List, NewRest).
get-any(A, [A | Rest], Rest).
	
perform_action(PrevState, move(X, Y, Z), NextState) :- 
	delete(PrevState, clear(Z), MidS1),
	delete(MidS1, on(X, Y), MidS2),
	append([on(X, Z), clear(Y)], MidS2, NextState).	
	
	

	
plan(State, Goals, [ ], State, _) :-
	writeln('procedura goals_achieved->'),
	goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState, Int):-
	writeln(' '),
	write('POZIOM '),
	writeln(Int),
	write('Cele:  '),
	writeln(Goals),
	
	NewInt is Int+1,
	
	writeln('procedura choose_goal->'),
	choose_goal(Goal, Goals, RestGoals, InitState),
	
	writeln('procedura achives->'),
	achieves(Action, Goal, Addit),
	writeln(Action),
	
	writeln('procedura requires->'),
	requires(Action, Conditions, Addit),
	write('Pod-Cele: '),
	writeln(Conditions),
	
	writeln('procedura plan-1->'),
	plan(InitState, Conditions, PrePlan, MidState1, NewInt),
	
	writeln('procedura perform_action->'),
	perform_action(MidState1, Action, MidState2),
	
	write('Reszta Celow: '),
	writeln(RestGoals),
	writeln('procedura plan-2->'),
	plan(MidState2, RestGoals, PostPlan, FinalState, NewInt),
	
	writeln(' '),
	write('Powrot >> POZIOM '),
	writeln(Int),
	
	writeln('procedura append->'),
	append(PrePlan, [ Action | PostPlan ], Plan), ! .

	
plan(_, _, _, _, Int):-
	write('plan Poziom '),
	write(Int),
	writeln(' FAIL !!!!!!!!!').