day(7). testResult(40).

:- use_module(lib/solve), table(timelines/3).

tPrev(H, T, Pos, Count) :-
  P is Pos - 1,
  nth0(P, H, '^')
  -> timelines(T, P, Count)
  ; Count = 0.  
tCur(H, T, Pos, Count) :-
  nth0(Pos, H, '.')
  -> timelines(T, Pos, Count)
  ; Count = 0.  
tNext(H, T, Pos, Count) :-
  P is Pos + 1,
  nth0(P, H, '^')
  -> timelines(T, P, Count)
  ; Count = 0.  

timelines([H], Pos, 1) :- nth0(Pos, H, 'S'), !.
timelines([_], _, 0) :- !.
timelines([H|T], Pos, Timelines) :-
  tPrev(H, T, Pos, Cp),
  tCur(H, T, Pos, Cc),
  tNext(H, T, Pos, Cn),
  Timelines is Cp + Cc + Cn.

result(Manifold, Timelines) :-
  reverse(Manifold, ReverseManifold),
  length(ReverseManifold, Length),
  aggregate_all(sum(C), (between(0, Length, P), timelines(ReverseManifold, P, C)), Timelines).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
