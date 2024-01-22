#!/usr/bin/env swipl

:- include('indp.pl').

:- initialization(main, main).

main(Argv) :-
    writeln(Argv),
    Argv = [F], !,
    filename_fofs_run(F).
main(_Argv) :-
    writeln('Error, usage: indp.sh file'),
    writeln('Process file.pl and file.pr to generate file<i>.fof for each lemma in file.pr').
