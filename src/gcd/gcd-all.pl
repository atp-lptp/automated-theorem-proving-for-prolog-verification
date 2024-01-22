:- op(700,xfy,@<).		% less (nat)
:- op(700,xfy,@=<).		% less than or equal (nat)

gcd(X,Y,D) :-
    X @=< Y, gcd_leq(X,Y,D).
gcd(X,Y,D) :-
    Y @< X, gcd_leq(Y,X,D).

gcd_leq(0,Y,Y).
gcd_leq(s(X),Y,D) :-
    plus(s(X),Z,Y),
    gcd(s(X),Z,D).
  
/*       
gcd(X,Y,D) :-
	(	X @=< Y ->
		gcd_leq(X,Y,D)
	;	gcd_leq(Y,X,D)
	).

gcd_leq(X,Y,D) :-
	(	X = 0 ->
		D = Y
	;	plus(X,Z,Y),
		gcd(X,Z,D)
	).
*/

% We use @< and @=< to distinguish these predicates from the built-in
% predicates < and =<.

nat(0).
nat(s(X)) :- nat(X).

0 @< s(Y).
s(X) @< s(Y) :- X @< Y.

0 @=< Y.
s(X) @=< s(Y) :- X @=< Y.

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

times(0,Y,0).
times(s(X),Y,Z) :- times(X,Y,A), plus(Y,A,Z).

nat_list([]).
nat_list([X|L]) :- nat(X), nat_list(L).

