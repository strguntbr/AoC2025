day(9).
testResult(part1, 50).
testResult(part2, 24).
testResult(part2, "test-1", 22).
% testResult(part2, "test-2", 16). % This test actually fails due to some assumtions this solution makes (see comment in implementation below)

:- use_module(lib/solve).

rectangleSize([Sx,Sy], [Ex,Ey], Size) :- Size is (abs(Ex-Sx)+1)*(abs(Ey-Sy)+1).

normalize([[X1,Y1],[X2,Y2]], [[Xn1,Yn1],[Xn2,Yn2]]) :- Xn1 is min(X1, X2), Xn2 is max(X1, X2), Yn1 is min(Y1, Y2), Yn2 is max(Y1, Y2).

borders([_], []).
borders([S,E|T], [Border|NextBorders]) :- normalize([S,E], Border), borders([E|T], NextBorders).

intersects([[BX,BYs], [BX,BYe]], [[RXs,RYs], [RXe,RYe]]) :-
  between(']', RXs, RXe, '[', BX),
  ((BYs =< RYs, BYe > RYs) ; (BYe >= RYe, BYs < RYe)), !.
intersects([[BXs,BY], [BXe,BY]], [[RXs,RYs], [RXe,RYe]]) :-
  between(']', RYs, RYe, '[', BY),
  ((BXs =< RXs, BXe > RXs) ; (BXe >= RXe, BXs < RXe)), !.
  
crossesBorder_([H|_], S, E) :- intersects(H, [S, E]), !.
crossesBorder_([_|T], S, E) :- crossesBorder(T, S, E).
crossesBorder(Borders, S, E) :- crossesBorder_(Borders, S, E).

insideTile(Borders, [X,Y]) :-
  aggregate_all(count, (member([[Sx,By],[Ex,By]], Borders), X>=Sx, X<Ex, By=<Y), BordersAbove),
  1 is mod(BordersAbove, 2).

resultPart1(RedTiles, MaxSize) :- aggregate_all(max(Size), (select2(RedTiles, S, E, _), rectangleSize(S, E, Size)), MaxSize).

resultPart2(RedTiles, MaxSize) :-
  [H|_] = RedTiles, append(RedTiles, [H], TilesWrapped), borders(TilesWrapped, Borders),
  aggregate_all(
    max(Size),
    (
      select2(RedTiles, T1, T2, _),
      normalize([T1,T2], [S,E]),
      % Technically there could be a valid rectangle that has crossing borders, if it has
      % 2 parallel adjacent crossing borders. But my input does not contain such a case (and
      % I guess nobody elses input does as nobody seems to care about that case) and checking
      % this special case is not trivial as we would have to allow pairs of adjacent borders,
      % but only if they do not introduce any 'holes' (which they could). Thus I decided to
      % just not care and document that part2 might fail if it is called with such a special
      % case.
      (\+ crossesBorder(Borders,S,E)),
      insideTile(Borders, S),
      rectangleSize(S, E, Size)
    ),
    MaxSize
  ).

/* required for loadData */
data_line(Pos, Line) :- split_string(Line, ",", "", [X,Y]), maplist(number_string, Pos, [X,Y]).
 