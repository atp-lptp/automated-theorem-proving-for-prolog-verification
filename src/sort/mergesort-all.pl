mergesort([],[]).
mergesort([X],[X]).
mergesort([X,Y|Xs],Ys) :-
	split([X,Y|Xs],Xs1,Xs2),
	mergesort(Xs1,Ys1),
	mergesort(Xs2,Ys2),
	merge(Ys1,Ys2,Ys).

split([],[],[]).
split([X|Xs],[X|Ys],Zs) :- split(Xs,Zs,Ys).

merge([],Xs,Xs).
merge(Xs,[],Xs).
merge([X|Xs],[Y|Ys],[Z|Zs]) :-
	X =< Y, Z = X, merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[Z|Zs]) :-
	\+ (X =< Y), Z = Y, merge([X|Xs],Ys,Zs).

int_list([]).
int_list([X|L]) :-
	integer(X),
	int_list(L).

int_ordered([]).
int_ordered([X]).
int_ordered([X,Y|L]) :-
	X =< Y,
	int_ordered([Y|L]).


%%%%

list([]).
list([X|L]) :- list(L).

member(X,[X|L]).
member(X,[Y|L]) :- member(X,L).

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

length([],0).
length([X|L],s(N)) :- length(L,N).

delete(X,[X|L],L).
delete(X,[Y|L1],[Y|L2]) :- delete(X,L1,L2).



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

permutation([],[]).
permutation(L1,[X|L3]) :-
	delete(X,L1,L2),
	permutation(L2,L3).

same_occ(L1,L2) :- \+ not_same_occ(L1,L2).

not_same_occ(L1,L2) :-
	member2(X,L1,L2),
	occ(X,L1,N1),
	occ(X,L2,N2),
	\+ (N1 = N2).

occ(X,[],0).
occ(X,[X|L],s(N)) :- occ(X,L,N).
occ(X,[Y|L],N) :- \+ (X = Y), occ(X,L,N).

member2(X,L1,L2) :- member(X,L1).
member2(X,L1,L2) :- member(X,L2).


