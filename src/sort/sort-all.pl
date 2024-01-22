permutation_sort(L1,L2) :-
	permutation(L1,L2),
	ordered(L2).

ordered([]).
ordered([X]).
ordered([X,Y|L]) :- X @=< Y, ordered([Y|L]).

insert_sort([],[]).
insert_sort([X|L1],L3) :-
	insert_sort(L1,L2),
	insert(X,L2,L3).
	
insert(X,[],[X]).
insert(X,[Y|L],[X,Y|L]) :- X @=< Y.
insert(X,[Y|L1],[Y|L2]) :- not(X @=< Y), insert(X,L1,L2).

quick_sort(L1,L2) :- quick_sort(L1,[],L2).

quick_sort([],L,L).
quick_sort([X|L1],L2,L6) :-
	split(X,L1,L3,L4),
	quick_sort(L3,L2,L5),
	quick_sort(L4,[X|L5],L6).

split(X,[],[],[]).
split(X,[Y|L1],[Y|L2],L3) :- X @=< Y, split(X,L1,L2,L3).
split(X,[Y|L1],L2,[Y|L3]) :- not(X @=< Y), split(X,L1,L2,L3).

/*
split(X,[],[],[]).
split(X,[Y|L1],L2,L3) :-
	(	X @=< Y -> 
		L2 = [Y|L4], 
		L3 = L5
	; 	L2 = L4, 
		L3 = [Y|L5]
	),
	split(X,L1,L4,L5).
*/		


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


