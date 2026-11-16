% even/1 and odd/1 are mutually recursive
% (R. F. Staerk, First-order theories for pure Prolog programs with negation,
% Example 10.1).
even(0).
even(s(X)) :- odd(X).

odd(s(X)) :- even(X).

nat(0).
nat(s(X)) :- nat(X).
