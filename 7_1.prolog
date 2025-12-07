day(7). testResult(21).

:- use_module(lib/solve).

nextBeams([_|[]], [_|[]], [], 0).
nextBeams([_     |Tp], [_,'S' |Tc], [l|Tn], Splits) :- !, nextBeams(       Tp,    [.|Tc], Tn, Splits).
nextBeams([_,Hp,l|Tp], [_,Hc,^|Tc], [l|Tn], Splits) :- !, nextBeams([Hp,l|Tp], [Hc,^|Tc], Tn, Sn), Splits is Sn + 1.
nextBeams([l,Hp  |Tp], [^,Hc  |Tc], [l|Tn], Splits) :- !, nextBeams(  [Hp|Tp],   [Hc|Tc], Tn, Splits).
nextBeams([_, l  |Tp], [_,'.' |Tc], [l|Tn], Splits) :- !, nextBeams(   [l|Tp],    [.|Tc], Tn, Splits).
nextBeams([_     |Tp], [_     |Tc], [.|Tn], Splits) :- !, nextBeams(       Tp,        Tc, Tn, Splits).

beamSplits(_, [], 0).
beamSplits(PrevBeams, [H|T], Splits) :-
  nextBeams([.|PrevBeams], [.|H], NextBeams, CurSplits),
  beamSplits(NextBeams, T, NextSplits),
  Splits is NextSplits + CurSplits.

result([H|T], Result) :-
  beamSplits(H, [H|T], Result).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
