day(5). groupData. testResult(14).

:- use_module(lib/solve).

assertFresh(Start, End) :-
  S is Start, E is End,
  \+ (freshRange([S, E]) ; S > E) -> assertz(freshRange([S, E])) ; true.

removeRange([Rs, Re], [Fs, Fe]) :-
  between(Rs, Re, Fs), !,/* [Rs,Re] starts before and overlaps */
  retractall(freshRange([Fs, Fe])),
  assertFresh(Re+1, Fe).
removeRange([Rs, Re], [Fs, Fe]) :-
  between(Fs, Fe, Rs), !,/* [Fs,Fe] starts before and overlaps or covers the whole range */
  retractall(freshRange([Fs, Fe])),
  assertFresh(Fs, Rs-1),
  assertFresh(Re+1, Fe).
removeRange(_, _).

removeFromAllRanges(Range) :-
  retractall(freshRange(Range)),
  forall(freshRange(OtherRange), removeRange(Range, OtherRange)).

countFreshIngredients(Count) :-
  freshRange([S,E]), !,
  removeFromAllRanges([S,E]),
  countFreshIngredients(NextCount),
  Count is NextCount + (E-S+1).
countFreshIngredients(0).

result(_, FreshIngredients) :- countFreshIngredients(FreshIngredients).
  
/* required for loadData */
data_line(Range, Line) :- 
  split_string(Line, "-", "", [Start,End]), !,
  maplist(number_string, Range, [Start,End]),
  assert(freshRange(Range)).
data_line(Ingredient, Line) :- number_string(Ingredient, Line), !.
data_line([], "").
