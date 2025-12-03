day(3). testResult(part1, 357). testResult(part2, 3121910778619).

:- use_module(lib/solve), use_module(lib/math).

bestBatteries(Batteries, Count, Batteries) :- length(Batteries, Count), !.
bestBatteries(_, 0, []) :- !.
bestBatteries([Bh|Bt], Count, BestBatteries) :-
  bestBatteries(Bt, Count, [Nbbh|Nbbt]),
  (
    Bh < Nbbh
    -> BestBatteries = [Nbbh|Nbbt]
    ; (
      NextCount is Count - 1,
      bestBatteries([Nbbh|Nbbt], NextCount, NextBestBatteries),
      BestBatteries = [Bh|NextBestBatteries]
    )
  ).

totalOutputJoltage(Banks, BatteryCount, OutputJoltage) :-
  maplist([Bank,Best]>>bestBatteries(Bank,BatteryCount,Best), Banks, BestBatteries),
  maplist(number_digits, Joltages, BestBatteries),
  sumlist(Joltages, OutputJoltage).

resultPart1(Banks, OutputJoltage) :- totalOutputJoltage(Banks, 2, OutputJoltage).
resultPart2(Banks, OutputJoltage) :- totalOutputJoltage(Banks, 12, OutputJoltage).

/* required for loadData */
data_line(Bank, Line) :- string_chars(Line, Chars), maplist(atom_number, Chars, Bank).
