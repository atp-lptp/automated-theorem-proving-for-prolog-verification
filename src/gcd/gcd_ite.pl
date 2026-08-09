:- op(700,xfy,@<).		% less (nat)
:- op(700,xfy,@=<).		% less than or equal (nat)

% Variant of ./gcd.pl using the ISO if-then-else construct.
%
% Kept side by side with the two-clause version on purpose: both define the
% same relation (checked exhaustively for 0..10 x 0..10), but they do not
% produce the same completion, and the provers do not behave the same on them.
% See ./README-gcd-variants.md.

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

/*
gcd(X,Y,D) :-
    X @=< Y, gcd_leq(X,Y,D).
gcd(X,Y,D) :-
    Y @< X, gcd_leq(Y,X,D).

gcd_leq(0,Y,Y).
gcd_leq(s(X),Y,D) :-
    plus(s(X),Z,Y),
    gcd(s(X),Z,D).
*/