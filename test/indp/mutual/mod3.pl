% zero/1, one/1 and two/1 are mutually recursive:
% the natural numbers that are 0, 1 and 2 modulo 3.
zero(0).
zero(s(X)) :- two(X).

one(s(X)) :- zero(X).

two(s(X)) :- one(X).

nat(0).
nat(s(X)) :- nat(X).
