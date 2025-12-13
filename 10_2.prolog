day(10). testResult(33).

:- use_module(library(clpfd)), use_module(library(debug)), use_module(lib/solve).

buttonMatrix(_, _, [], []).
buttonMatrix(Index, [Index|OtherButtons], [_|JoltageRequirementsTail], [1|MatrixTail]) :- !,
  NextIndex is Index + 1,
  buttonMatrix(NextIndex, OtherButtons, JoltageRequirementsTail, MatrixTail).
buttonMatrix(Index, Buttons, [_|JoltageRequirementsTail], [0|MatrixTail]) :- !,
  NextIndex is Index + 1,
  buttonMatrix(NextIndex, Buttons, JoltageRequirementsTail, MatrixTail).

prepareMachine([Buttons, JoltageRequirements], [Matrices, JoltageRequirements]) :-
  maplist({JoltageRequirements}/[B,M]>>buttonMatrix(0,B,JoltageRequirements,M), Buttons, Matrices).
  
calculateJoltageLevels(_, [], _).
calculateJoltageLevels(Buttons, [RequiredLevel|OtherLevels], ButtonPresses) :-
  maplist([H,[H|T],T]>>true, Connections, Buttons, NextButtons),
  scalar_product(Connections, ButtonPresses, #=, RequiredLevel),
  calculateJoltageLevels(NextButtons, OtherLevels, ButtonPresses).

initMaxPresses([], _).
initMaxPresses([H|T], Max) :-
  H in 0..Max,
  initMaxPresses(T, Max).

findConstraints(Buttons, JoltageRequirements, MinPresses, MaxPresses) :-
  /* each button press can only increment a counter by one, thus we need at least as much presses as the hightest required joltage level */
  aggregate_all(max(Joltage), member(Joltage, JoltageRequirements), MinPresses),
  /* the maximum of presses is the sum of all joltages divided by the number of lights the smalles button increases */
  sumlist(JoltageRequirements, JoltageSum),
  aggregate_all(min(Lights), (member(Button,Buttons), sumlist(Button,Lights)), SmallestButton),
  MaxPresses is div(JoltageSum, SmallestButton).

configureMachine([Buttons, JoltageRequirements], ButtonPresses, TotalPresses) :-
  same_length(Buttons, ButtonPresses),
  findConstraints(Buttons, JoltageRequirements, MinPresses, MaxPresses),
  TotalPresses in MinPresses..MaxPresses,
  initMaxPresses(ButtonPresses, MaxPresses),
  calculateJoltageLevels(Buttons, JoltageRequirements, ButtonPresses),
  sum(ButtonPresses, #=, TotalPresses),
  labeling([min(TotalPresses),ffc,down,bisect], [TotalPresses|ButtonPresses]).

result(Manual, Result) :-
  maplist(prepareMachine, Manual, Machines),
  concurrent_maplist(configureMachine, Machines, _, MinPresses),
  sumlist(MinPresses, Result).

/* required for loadData */
data_line([Buttons, JoltageRequirements], Line) :-
  split_string(Line, " ", "[]", [_|T]),
  append(ButtonsStr, [JoltageStr], T),
  maplist(string_buttons, ButtonsStr, Buttons),
  string_buttons(JoltageStr, JoltageRequirements).
string_buttons(String, Buttons) :-
  split_string(String, ",", "(){}", Strings),
  maplist(number_string, Buttons, Strings).
