day(4). testResult(part1, 13). testResult(part2, 43).

:- use_module(lib/solve).

neighboringRoll(X, Y, Xn, Yn) :-
  Xs is X - 1, Xe is X + 1, Ys is Y - 1, Ye is Y + 1,
  between(Xs, Xe, Xn), between(Ys, Ye, Yn),
  [X,Y] \= [Xn,Yn],
  roll(Xn, Yn).
accessibleRoll(X, Y) :-
  roll(X, Y),
  findall([Xn,Yn], neighboringRoll(X, Y, Xn, Yn), N),
  length(N, L), L < 4.
removeAccessibleRoll(X, Y) :- accessibleRoll(X, Y), retract(roll(X, Y)).

resultPart1(_, Count) :-
  findall([X,Y], accessibleRoll(X, Y), Rolls),
  length(Rolls, Count).
resultPart2(Data, Count) :-
  findall([X,Y], removeAccessibleRoll(X, Y), Rolls),
  length(Rolls, C),
  (C = 0 -> Count = 0 ; resultPart2(Data, Cn), Count is C + Cn).

/* required for loadData */
data_line(Index, [], Line) :- string_chars(Line, Data), assertRolls(Index, 1, Data).

assertRolls(_, _, []).
assertRolls(X, Y, [H|T]) :- assertRoll(X, Y, H), Yn is Y+1, assertRolls(X, Yn, T).
assertRoll(X, Y, E) :- E \= '@' ; assert(roll(X, Y)).
