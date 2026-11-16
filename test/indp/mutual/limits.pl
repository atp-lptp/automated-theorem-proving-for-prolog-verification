% Limit cases of mutual induction: nat/1 and list/1 are not mutually recursive.
nat(0).
nat(s(X)) :- nat(X).

list([]).
list([X|L]) :- list(L).
