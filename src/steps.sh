#!/usr/bin/env swipl

:- include('indp.pl').
:- include('steps.pl').

:- initialization(main, main).

main([F]) :- !,
    filename_step_fofs(F).
main([F,A]) :-
    atom_number(A,I), integer(I), !,
    filename_indice_step_fofs(F,I).
main(_Argv) :-
    writeln('Error, usage: steps.sh file [i]'),
    writeln('Process file.pl and file.pr to generate file<i>_step<k>.fof,'),
    writeln('one file per induction step, for each lemma in file.pr (or for lemma i only).').
