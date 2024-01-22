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