day(11). testResult(part1, 5). testResult(part2, "test-2", 2).

:- use_module(lib/solve), dynamic(attached/2), table(paths/3).

paths(Target, Target, 1) :- !.
paths(Start, Target, Length) :- aggregate_all(sum(Ln), (attached(Start, Next), paths(Next, Target, Ln)), Length).

pathChain([_], 1).
pathChain([S,T|R], Count) :-
  paths(S, T, C1),
  pathChain([T|R], C2),
  Count is C1 * C2.

resultPart1(_, Count) :- paths("you", "out", Count).
resultPart2(_, Count) :-
  pathChain(["svr", "fft", "dac", "out"], C1),
  pathChain(["svr", "dac", "fft", "out"], C2),
  Count is C1 + C2.

/* required for loadData */
data_line([Start,Ends], Line) :- 
  split_string(Line, " ", ":", [Start|Ends]),
  foreach(member(E, Ends), assert(attached(Start,E))).
