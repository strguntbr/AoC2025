day(1). testResult(6).

:- use_module(lib/solve).

rotate(Pos, Steps, NextPos, Zeros) :-
  Steps < 0, !, /* if rotating counter clockwiese invert everything and calcualte like clockwise rotation to avoid handlind special cases when crossing 0 */
  InvPos is mod(100 - Pos, 100), InvSteps is 0 - Steps,
  rotate(InvPos, InvSteps, InvNextPos, Zeros),
  NextPos is mod(100 - InvNextPos, 100).
rotate(Pos, Steps, NextPos, Zeros) :-
  NextPos is mod(Steps + Pos, 100),
  Zeros is abs(div(Steps + Pos, 100)).

count0Clicks(_, [], 0).
count0Clicks(Pos, [H|T], FinalZeros) :-
  rotate(Pos, H, NextPos, Zeros),
  count0Clicks(NextPos, T, NextZeros),
  FinalZeros is Zeros + NextZeros.

result(Instructions, Password) :- count0Clicks(50, Instructions, Password).

/* required for loadData */
data_line(Rotate, Line) :- string_concat("L", NumberString, Line), number_string(Count, NumberString), Rotate is 0-Count.
data_line(Rotate, Line) :- string_concat("R", NumberString, Line), number_string(Rotate, NumberString).
