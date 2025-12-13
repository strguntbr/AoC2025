day(10). testResult(7).

:- use_module(library(debug)), use_module(lib/solve).

toggleLight(Index, LightRequirement, NextLightRequirements) :-
  length(Prefix, Index),
  append(Prefix, [Light|Suffix], LightRequirement),
  select(Light, ['#','.'], [ToggledLight]),
  append(Prefix, [ToggledLight|Suffix], NextLightRequirements).

pressButton([], LightRequirements, LightRequirements).
pressButton([H|T], LightRequirements, FinalLightRequirements) :-
  toggleLight(H, LightRequirements, NextLightRequirements),
  pressButton(T, NextLightRequirements, FinalLightRequirements).

configure(LightRequirements, _, 0) :- forall(member(L, LightRequirements), L='.').
configure(LightRequirements, [H|T], Count) :-
  pressButton(H, LightRequirements, NextLightRequirements),
  configure(NextLightRequirements, T, NextCount),
  Count is NextCount + 1.
configure(LightRequirements, [_|T], Count) :- configure(LightRequirements, T, Count).

configureLights([LightRequirements, Buttons], ButtonsPressed) :-
  aggregate_all(min(Press), configure(LightRequirements, Buttons, Press), ButtonsPressed).

result(Machines, Result) :-
  concurrent_maplist(configureLights, Machines, ButtonsPressed),
  sumlist(ButtonsPressed, Result).

/* required for loadData */
data_line([Lights, Buttons], Line) :-
  split_string(Line, " ", "[]", [LightsStr|T]),
  append(ButtonsStr, [_], T),
  string_lights(LightsStr, Lights),
  maplist(string_buttons, ButtonsStr, Buttons).
string_lights(String, Lights) :-
  string_chars(String, Lights).
string_buttons(String, Buttons) :-
  split_string(String, ",", "()", Strings),
  maplist(number_string, Buttons, Strings).
