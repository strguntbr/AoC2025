:- module(math, [
  gcd/3,
  lcm/3,
  lcmall/2,
  number_digits/2
]).

gcd(0, GCD, GCD) :- !.
gcd(X, Y, GCD) :- X < Y, NextY is Y mod X, gcd(X, NextY, GCD).
gcd(X, Y, GCD) :- X > Y, gcd(Y, X, GCD).

lcm(X, Y, LCM):-gcd(X, Y, GCD), LCM is X*Y/GCD.

lcmall([H], H).
lcmall([H|T], LCM) :- lcmall(T, LN), lcm(H, LN, LCM).

number_digits(Number, Digits) :-
  \+ var(Digits), !,
  reverse(Digits, ReverseDigits),
  reverseDigitsToNumber(ReverseDigits, Number).
number_digits(Number, Digits) :-
  \+ var(Number), Number >= 0,
  numberToReverseDigits(Number, ReverseDigits),
  reverse(ReverseDigits, Digits).
reverseDigitsToNumber([], 0) :- !.
reverseDigitsToNumber([H|T], Number) :-
  reverseDigitsToNumber(T, NextNumber),
  Number is NextNumber * 10 + H.
numberToReverseDigits(0, []) :- !.
numberToReverseDigits(Number, [H|T]) :-
  H is mod(Number, 10),
  NextNumber is div(Number, 10),
  numberToReverseDigits(NextNumber, T).