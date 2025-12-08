day(8). auxData(part1, 1000). testResult(part1, auxData(10), 40). testResult(part2, 25272).

:- use_module(lib/solve).

dist([X1,Y1,Z1], [X2,Y2,Z2], D) :-
  D is (X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2.

connections([], []).
connections([H|T], Connections) :-
  findall(c{d: D, s: H, e: E}, (member(E, T), dist(H, E, D)), CurConnections),
  connections(T, NextConnections),
  append(CurConnections, NextConnections, Connections).
sortedConnections(Boxes, SortedConnections) :-
  connections(Boxes, Connections),
  sort(d, @=<, Connections, SortedConnections).

removeCircuit([H|T], C, H, T) :- member(C, H.m), !.
removeCircuit([H|T], C, M, [H|Tn]) :- removeCircuit(T, C, M, Tn).

mergeCircuits(C1, C2, circuit{s: S, m: M}) :-
  S is C1.s + C2.s,
  append(C1.m, C2.m, M).

connect(Circuits, [], Circuits, []) :- !.
connect([Circuit], _ ,[Circuit], []) :- !.
connect(Circuits, [H|T], ConnectedCircuits, [H|Ct]) :-
  removeCircuit(Circuits, H.s, C1, Cn1),
  (
    member(H.e, C1.m)
    -> (
      Cc = C1, NextConnectedCircuits = Cn1
    )
    ; (
      removeCircuit(Cn1, H.e, C2, NextConnectedCircuits),
      mergeCircuits(C1, C2, Cc)
    )
  ),
  connect([Cc|NextConnectedCircuits], T, ConnectedCircuits, Ct).

resultPart1(Boxes, Count, Result) :-
  sortedConnections(Boxes, SortedConnections),
  length(ShortestConnections, Count), append(ShortestConnections, _, SortedConnections),
  maplist([B,circuit{s: 1, m: [B]}]>>true, Boxes, Circuits),
  connect(Circuits, ShortestConnections, ConnectedCircuits, _),
  maplist([circuit{m:_, s: S},S]>>true, ConnectedCircuits, CircuitSizes),
  sort(0, @>=, CircuitSizes, [S1,S2,S3|_]),
  Result is S1 * S2 * S3.

resultPart2(Boxes, Distance) :-
  sortedConnections(Boxes, SortedConnections),
  maplist([B,circuit{s: 1, m: [B]}]>>true, Boxes, Circuits),
  connect(Circuits, SortedConnections, _, UsedConnections),
  last(UsedConnections, c{d:_, s:[Xs,_,_], e: [Xe,_,_]}),
  Distance is Xs * Xe.

/* required for loadData */
data_line(Coordinates, Line) :- split_string(Line, ",", " ", [X,Y,Z]), maplist(number_string, Coordinates, [X,Y,Z]).
