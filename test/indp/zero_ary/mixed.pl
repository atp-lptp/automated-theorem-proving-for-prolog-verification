% 0-ary and n-ary predicates together.
p :- q, p.
q.
n(0).
n(s(X)) :- n(X).
r :- n(s(0)).
