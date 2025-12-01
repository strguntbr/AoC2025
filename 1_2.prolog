day(1). testResult(6).

:- use_module(lib/solve).

rotate(Pos, 0, Pos, 0) :- !.
rotate(Pos, Steps, FinalPos, Zeros) :-
  Step is Steps/abs(Steps),
  NextSteps is Steps - Step,
  NextPos is mod(Pos + Step, 100),
  rotate(NextPos, NextSteps, FinalPos, NextZeros),
  (NextPos = 0 -> Zeros is NextZeros + 1 ; Zeros = NextZeros).

count0Clicks(_, [], 0).
count0Clicks(Pos, [H|T], FinalZeros) :-
  rotate(Pos, H, NextPos, Zeros),
  count0Clicks(NextPos, T, NextZeros),
  FinalZeros is Zeros + NextZeros.

result(Instructions, Password) :- count0Clicks(50, Instructions, Password).

/* required for loadData */
data_line(Rotate, Line) :- string_concat("L", NumberString, Line), number_string(Count, NumberString), Rotate is 0-Count.
data_line(Rotate, Line) :- string_concat("R", NumberString, Line), number_string(Rotate, NumberString).
