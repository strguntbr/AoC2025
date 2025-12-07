day(6). testResult(3263827).

:- use_module(lib/solve), use_module(lib/matrix).

initial('+', 0).
initial('*', 1).

eval(A, '+', B, R) :- R is A + B.
eval(A, '*', B, R) :- R is A * B.

chars_number(Chars, Number) :-
  string_chars(String, Chars),
  split_string(String, " ", " *+", [NumberStr]),
  number_string(Number, NumberStr). 

isOp('+').
isOp('*').
isEmpty(' ').

calcAll([], Op, CurResult, 0) :- initial(Op, CurResult).
calcAll([H|T], Op, CurResult, GrandTotal) :-
  forall(member(E, H), isEmpty(E)), !, 
  initial(Op, CurResult),
  calcAll(T, Result, NextGrandTotal),
  GrandTotal is NextGrandTotal + Result.
calcAll([H|T], Op, CurResult, GrandTotal) :-
  calcAll(T, Op, NextResult, GrandTotal),
  chars_number(H, Number),
  eval(NextResult, Op, Number, CurResult).

calcAll([H|T], CurResult, GrandTotal) :-
  append(NumberChars, [Op], H),
  isOp(Op),
  calcAll(T, Op, NextResult, GrandTotal),
  chars_number(NumberChars, Number),
  eval(NextResult, Op, Number, CurResult).

result(Homework, GrandTotal) :- 
  transpose(Homework, Transposed),
  calcAll(Transposed, CurResult, NextGrandTotal),
  GrandTotal is NextGrandTotal + CurResult.

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
