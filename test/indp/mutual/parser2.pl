% An efficient parser for expressions: exp/2, term/2, factor/2, moreexps/2
% and moreterms/2 are mutually recursive, with negation (R. F. Staerk,
% First-order theories for pure Prolog programs with negation, Fig. 5).
% exp/1, term/1 and factor/1 are the simple parser of Fig. 3.
exp(A,C) :- term(A,B), moreexps(B,C).

term(A,C) :- factor(A,B), moreterms(B,C).

factor([lpar|A],B) :- exp(A,[rpar|B]).
factor([l(X)|A],A).

moreexps([+|A],C) :- term(A,B), moreexps(B,C).
moreexps(A,A) :- \+ plus(A).

moreterms([*|A],C) :- factor(A,B), moreterms(B,C).
moreterms(A,A) :- \+ times(A).

plus([+|X]).

times([*|X]).

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
