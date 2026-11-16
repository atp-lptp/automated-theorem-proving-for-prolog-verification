% A simple parser for expressions: exp/1, term/1 and factor/1 are mutually
% recursive (R. F. Staerk, First-order theories for pure Prolog programs with
% negation, Fig. 3). A token is l(X), +, *, lpar or rpar.
exp(C) :- append(A,[+|B],C), exp(A), term(B).
exp(C) :- term(C).

term(C) :- append(A,[*|B],C), term(A), factor(B).
term(C) :- factor(C).

factor(C) :- append([lpar|A],[rpar],C), exp(A).
factor([l(X)]).

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

list([]).
list([X|L]) :- list(L).
