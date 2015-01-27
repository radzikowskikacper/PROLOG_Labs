goals_achieved([],State).
goals_achieved([Goal|RestGoals], State) :-
	check_goal(Goal, State),
	goals_achieved(RestGoals, State).

check_goal(Goal, State):-
	parse(Goal, [], GoalList, Diff),
	subset(GoalList, State),
	check_diff(Diff).

check_diff([]).
check_diff([diff(X1,X2)|Rest]):-
	X1\=X2,
	check_diff(Rest).

parse(nil, Acc, Acc, []).
parse(clear(X/C), Acc, GoalList, Diff):-
	parse(C,[clear(X)|Acc], GoalList, Diff).
parse(clear(X), Acc, GoalList, Diff):-
	parse(nil, [clear(X)|Acc], GoalList, Diff).
parse(on(X1,X2/C), Acc, GoalList, Diff):-
	parse(C, [on(X1,X2)|Acc], GoalList, Diff).
parse(on(X1,X2), Acc, GoalList, Diff):-
	parse(nil, [on(X1,X2)|Acc], GoalList, Diff).
parse(diff(X1,X2), Acc, GoalList, [diff(X1,X2)|Diff]):-
	parse(nil, Acc, GoalList, Diff).


subset([],_).
subset([X|R1], State):-
	member(X, State),
	subset(R1, State).


member(X, [X|_]).
member(X, [Y|Rest]):-
	member(X, Rest).

choose_goal(ChosenGoal, [Goal|RestGoals], ReturnedGoals, State):-
	goals_achieved([Goal], State),
	choose_goal(ChosenGoal, RestGoals, ReturnedGoals, State).
choose_goal(Goal, [Goal|RestGoals], RestGoals, State).

achieves(move(X1, Y, X2), on(X1, X2/_)).
achieves(move(X1, Y, X2), on(X1, X2)).
achieves(move(X1, Y, X2), clear(Y/_)).
achieves(move(X1, Y, X2), clear(Y)).

requires(move(X1, X2, X3), [clear(X1), clear(X3/C)], on(X1, X3/C)).
requires(move(X1, X2, X3), [clear(X1), clear(X3/nil)], on(X1, X3)).
requires(move(X1, X2, X3), [clear(X1/on(X1,X2/C)), clear(X3/diff(X1,X3))], clear(X2/C)).
requires(move(X1, X2, X3), [clear(X1/on(X1,X2)), clear(X3/nil)], clear(X2)).

conc([],L2,L2).
conc([X|R1], L2, [X|RN]):-
	conc(R1, L2, RN).

delete(X,[X|R],R).
delete(X,[Y|R],[Y|R1]):-
	delete(X,R,R1).

perform_action(MidState1, move(X1,X2,X3), MidState2):-
	delete(clear(X3), MidState1, NewMidState1),
	delete(on(X1,X2), NewMidState1, NewMidState2),
	conc(NewMidState2, [on(X1,X3), clear(X2)], MidState2).


plan(State, Goals, [], State):-
	goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState):-
	choose_goal(Goal, Goals, RestGoals, InitState),
	achieves(Action, Goal),
	requires(Action, Conditions, Goal),
	plan(InitState, Conditions, PrePlan, MidState1),
	perform_action(MidState1, Action, MidState2),
	plan(MidState2, RestGoals, PostPlan, FinalState),
	conc(PrePlan, [Action, PostPlan], Plan).

main(Goals, Plan, FinalState):-
	plan([on( b4, p1), on( b1, b4),
	on(b3, b1), on(b2, p3),
	clear(b3), clear(b2),
	clear(p2), clear(p4) ],
	Goals, 
	Plan, 
	FinalState).