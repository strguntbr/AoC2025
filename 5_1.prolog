day(5). groupData. testResult(3).

:- use_module(lib/solve).

fresh(Ingredient) :- freshRange([Start, End]), between(Start, End, Ingredient), !.

result([_,Ingredients], FreshIngredients) :- aggregate_all(count, (member(I, Ingredients), fresh(I)), FreshIngredients).
  
/* required for loadData */
data_line(Range, Line) :- 
  split_string(Line, "-", "", [Start,End]), !,
  maplist(number_string, Range, [Start,End]),
  assert(freshRange(Range)).
data_line(Ingredient, Line) :- number_string(Ingredient, Line), !.
data_line([], "").
