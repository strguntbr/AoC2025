day(6). testResult(4277556).

:- use_module(lib/solve).

initial('+', 0).
initial('*', 1).

eval(A, '+', B, R) :- R is A + B.
eval(A, '*', B, R) :- R is A * B.

calcAll([Operators], Operators, Results) :- !, maplist(initial, Operators, Results).
calcAll([H|T], Operators, Results) :-
  calcAll(T, Operators, NextResults),
  maplist(eval, NextResults, Operators, H, Results).

result(Homework, GrandTotal) :- 
  calcAll(Homework, _, Results),
  sumlist(Results, GrandTotal).

/* required for loadData */
data_line(Numbers, Line) :- 
  split_string(Line, " ", " ", Data),
  maplist(number_string, Numbers, Data), !.
data_line(Operators, Line) :- 
  split_string(Line, " ", " ", Op),
  maplist(operator, Op, Operators).
operator("+", '+').
operator("*", '*').
