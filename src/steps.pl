%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The method by steps: one .fof file per induction step of a lemma.
%
% This is an alternative to the standard method of indp.pl (filename_fofs/1),
% which stays the default. To be loaded after indp.pl, e.g. through steps.sh.
%
% The induction axiom of a lemma is Closed => Sub, where Sub is the lemma.
% Closed is a conjunction, with one conjunct (one "step") per clause of the
% predicates the induction is about. A step file contains the axioms of the
% standard file, without the induction axiom, and one step as the conjecture.
% If every step is a theorem, so is Closed, hence the lemma.

% filename_step_fofs(+File)
%   - File.pl is a file name of a Prolog program P,
%     File.pr is an LPTP proof file about the Prolog code in File.pl
%  For each lemma i of File.pr that can be split into N steps, produces
%  File<i>_step<k>.fof for k = 1..N.
filename_step_fofs(File) :-
    filename_nbLemmas(File,N),
    forall(between(1,N,I), filename_indice_step_fofs(File,I)).

% filename_indice_step_fofs(+File,+I)
%  Produces the step files of the Ith lemma of File.pr and prints one line:
%    File<I>: <N> steps
%    File<I>: not applicable (<reason>)
filename_indice_step_fofs(File,I) :-
    filename_indice_steps(File,I,Result),
    (   Result = steps(Steps) ->
        standard_fof_lines(File,I,Lines),
        forall(nth1(K,Steps,Step), write_step_fof(File,I,K,Lines,Step)),
        length(Steps,N),
        format('~w~w: ~w steps~n',[File,I,N])
    ;   Result = not_applicable(Reason),
        format('~w~w: not applicable (~w)~n',[File,I,Reason])
    ).

% filename_indice_steps(+File,+I,-Result)
%   Result = steps(Steps), Steps being the induction steps of the Ith lemma
%       of File.pr, in LPTP syntax, or
%   Result = not_applicable(Reason).
% The method applies when the lemma is the conjunction of the Sub parts of
% its induction axioms.
filename_indice_steps(File,I,Result) :-
    filename_completion(File,Comp),
    filename_indice_defs_lemmas_induction_conjecture(File,I,_,_,_,InductionAxioms,lemma(_,FLemma)),
    (   \+ lemma_with_steps(FLemma,Comp) ->
        Result = not_applicable('shape of the lemma')
    ;   ( InductionAxioms == [] ; memberchk(lemma(induction,(true => _)),InductionAxioms) ) ->
        Result = not_applicable('no induction axiom')
    ;   findall(Step,
            (   member(lemma(induction,(Closed => _)),InductionAxioms),
                conjuncts(Closed,Cs),
                member(C,Cs),
                closed_step(C,Step) ),
            Steps),
        Result = steps(Steps)
    ),
    !.

    % lemma_with_steps(+FLemma,+Comp): the cases of name_fconjecture_comp_induction/4
    % where the lemma is the conjunction of the Sub parts of its induction axioms
    lemma_with_steps((all _:(_ => _)),_) :- !.
    lemma_with_steps((all _:(_ <=> _)),_) :- !.
    lemma_with_steps((L => R),_) :- zero_ary_induction_atom(L => R), !.
    lemma_with_steps((L <=> R),_) :- zero_ary_induction_atom(L => R), !.
    lemma_with_steps(FLemmas,Comp) :-
        FLemmas = (_ & _),
        conjuncts(FLemmas,FLemmas0),
        maplist(induction_lemma,FLemmas0,FLemmas1),
        mutual_induction(FLemmas1,Comp,_,_), !.

    % closed_step(+ClosedConjunct,-Step): on backtracking, the steps of
    %   all Vars : ((D1 \/ ... \/ Dk) => Phi), that is, all Vars : (Di => Phi)
    closed_step((all Vars : (SDP => Phi)),(all Vars : (D => Phi))) :- !, disjunct(SDP,D).
    closed_step((SDP => Phi),(D => Phi)) :- disjunct(SDP,D).

        disjunct((A \/ B),D) :- !, ( disjunct(A,D) ; disjunct(B,D) ).
        disjunct((ex V : G),(ex V : D)) :- !, disjunct(G,D).
        disjunct(D,D).

    % standard_fof_lines(+File,+I,-Lines): the lines of the standard .fof file
    % of the Ith lemma, without its induction axioms and its conjecture
    standard_fof_lines(File,I,Lines) :-
        setup_call_cleanup(
            tmp_file_stream(text,Tmp,Stream0),
            filename_indice_stream(File,I,Stream0),
            close(Stream0)),
        read_file_to_string(Tmp,String,[]),
        delete_file(Tmp),
        split_string(String,"\n","",AllLines),
        exclude(induction_or_conjecture_line,AllLines,Lines).

        induction_or_conjecture_line(Line) :- sub_string(Line,0,_,_,"fof(induction,"), !.
        induction_or_conjecture_line(Line) :- sub_string(Line,_,_,_,",conjecture,"), !.
        induction_or_conjecture_line("").

    write_step_fof(File,I,K,Lines,Step) :-
        format(atom(StepFof),'~w~w_step~w.fof',[File,I,K]),
        setup_call_cleanup(
            open(StepFof,write,Stream),
            (   forall(member(Line,Lines),(write(Stream,Line),nl(Stream))),
                write_lemma_as_TPTP_conjecture(Stream,lemma(step,Step)) ),
            close(Stream)).

/*
filename_step_fofs('../test/indp/mutual/even_odd').
filename_indice_step_fofs('../test/indp/mutual/even_odd',2).
filename_indice_steps('../test/indp/mutual/even_odd',2,R).
*/
