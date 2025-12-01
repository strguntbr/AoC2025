day(1). testResult(3).

:- use_module(lib/solve).

count0(_, [], 0).
count0(Pos, [H|T], Zeros) :-
  NextPos is mod(Pos + H, 100),
  count0(NextPos, T, NextZeros),
  (Pos = 0 -> Zeros is NextZeros + 1 ; Zeros = NextZeros).  

result(Instructions, Password) :- count0(50, Instructions, Password).

/* required for loadData */
data_line(Rotate, Line) :- string_concat("L", NumberString, Line), number_string(Count, NumberString), Rotate is 0-Count.
data_line(Rotate, Line) :- string_concat("R", NumberString, Line), number_string(Rotate, NumberString).
