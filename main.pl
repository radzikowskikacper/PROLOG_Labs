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
	
% niedeterministyczny  wybór celu 

choose_goal(Goal, [Goal | Rest], Rest, InitState) :- 
	not( member(Goal, InitState) ).
	
choose_goal(Goal, [ PrevGoal | Rest ], [ PrevGoal | NewRest ], InitState) :- 
	choose_goal(Goal, Rest, NewRest, InitState).
	
% END niedeterministyczny wybór celu	
	

achieves(move(X, _, Y), on(X, Y)).

achieves(move(X, A, Y), on(X, Y / on(Y, A))).

achieves(move(_, X, _), clear(X)):-
  atomic(X).

achieves(move(_, X, _), clear(X / on(X, _))):-
  not(atomic(X)).

  
requires(move(X, A, Z), [clear(X), clear(Z), on(X, A)], A):-
  atomic(X),!.

requires(move(X, Y, Z), [clear(X / on(X, Y)), clear(Z / nill)], Y):-
  atomic(Y).

requires(move(X, Y, Z), [clear(X / on(X, Y / on(Y, Target))), clear(Z / nill)], Target):-
  not(atomic(Y)).

  
goals_achieved([], _):- writeln('goals_achieved = ok').

goals_achieved([G|R], State) :-
	check_goal(G, State),
	goals_achieved(R, State).

check_goal(G, State):-
	writeln('procedura check_goal->'),
	parse(G, GList),
	write('Lista: '),
	writeln(GList),
	subset(GList, State). 

parse(nill, []).	
parse(clear(X), [clear(X)]).
parse(on(X, Y),  [on(X, Y)]).
	
parse(clear(X/C), [clear(X) | Rest]) :-
	parse(C, Rest).

parse(on(X, Y/C),  [on(X, Y) | Rest]) :-
	parse(C, Rest).


perform_action(PrevState, move(X, Y, Z), NextState) :- 
	delete(PrevState, clear(Z), MidS1),
	delete(MidS1, on(X, Y), MidS2),
	append([on(X, Z), clear(Y)], MidS2, NextState).	
	
	

	
plan(State, Goals, [ ], State, _) :-
	writeln('procedura goals_achieved->'),
	goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState, Int):-
	write('POZIOM '),
	writeln(Int),
	write('Cele:  '),
	writeln(Goals),
	
	NewInt is Int+1,
	
	writeln('procedura choose_goal->'),
	choose_goal(Goal, Goals, RestGoals, InitState),
	
	writeln('procedura achives->'),
	achieves(Action, Goal),
	
	writeln('procedura requires->'),
	requires(Action, Conditions, _),
	
	writeln('procedura plan-1->'),
	plan(InitState, Conditions, PrePlan, MidState1, NewInt),
	
	writeln('procedura perform_action->'),
	perform_action(MidState1, Action, MidState2),
	
	writeln('procedura plan-2->'),
	plan(MidState2, RestGoals, PostPlan, FinalState, NewInt),
	
	write('Powrót >> POZIOM '),
	writeln(Int),
	
	writeln('procedura append->'),
	append(PrePlan, [ Action | PostPlan ], Plan), ! .

	
plan(_, _, _, _, Int):-
	write('plan Poziom '),
	write(Int),
	writeln(' FAIL !!!!!!!!!').