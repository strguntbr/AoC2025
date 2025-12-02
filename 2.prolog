day(2). testResult(part1, 1227775554). testResult(part2, 4174379265).

:- use_module(lib/solve).

repeat(Prefix, Prefix, 1).
repeat(Prefix, Id, Repetions) :-
  string_concat(Prefix, Rest, Id),
  repeat(Prefix, Rest, RepetionsN),
  Repetions is RepetionsN + 1.

invalid(Id, Repetions) :-
  string_concat(Prefix, Rest, Id),
  string_length(Prefix, Length), Length > 0,
  repeat(Prefix, Rest, RepetionsN),
  Repetions is RepetionsN + 1.

invalidInRange(Ranges, Repetitions, Id) :-
  member([Start,End], Ranges), between(Start, End, Id),
  number_string(Id, Invalid),
  invalid(Invalid, Repetitions).

resultPart1([Ranges], Count) :-
  findall(Id, invalidInRange(Ranges, 2, Id), InvalidIds),
  sumlist(InvalidIds, Count).

resultPart2([Ranges], Count) :-
  findall(Id, invalidInRange(Ranges, _, Id), InvalidIds),
  sort(InvalidIds, UniqueInvalidIds), /* sorting to remove duplicates */
  sumlist(UniqueInvalidIds, Count).

/* required for loadData */
data_line(Ranges, Line) :- split_string(Line, ",", ",", RangeStrings), maplist(range, RangeStrings, Ranges).
range(String, [Start,End]) :- split_string(String, "-", "-", Range), maplist(number_string, [Start, End], Range).
