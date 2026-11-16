% p and q are mutually recursive (both define the natural numbers).
p(0).
p(s(X)) :- q(X).

q(X) :- p(X).

add(0,Y,Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).
