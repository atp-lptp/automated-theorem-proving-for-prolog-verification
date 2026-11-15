% Operator definitions from LPTP:

:- op(980,xfy,by).		    % ... by ...
:- op(970,xfy,:).		    % two Peano dots, right associative
:- op(960,yfx,<=>).		    % equivalence
:- op(950,xfy,=>).		    % implication (-> is an operator of Prolog)
:- op(940,yfx,\/).		    % disjunction (v can be used as name)
:- op(930,yfx,&).		    % conjunction
:- op(900,fy,~).		    % negation (cf. not, \+)
:- op(900,fy,not).		    % negation
:- op(900,fy,def).		    % def
:- op(900,fy,succeeds).		% succeeds
:- op(900,fy,fails).		% fails
:- op(900,fy,terminates).	% terminates
:- op(800,fy,all).		    % universal quantifier
:- op(800,fy,ex).		    % existential quantifier
:- op(700,yfx,=).		    % equality
:- op(700,xfy,<>).		    % different
:- op(700,xfy,<).		    % less (built-in)
:- op(700,xfy,=<).		    % less than or equal (built-in)
:- op(700,xfy,@<).		    % less (nat)
:- op(700,xfy,@=<).		    % less than or equal (nat)
:- op(700,xfy,#<).		    % less (int)
:- op(700,xfy,#=<).		    % less than or equal (int)
:- op(600,yfx,doubleslash).	% application of substitutions
:- op(600,yfx,**).		    % concatenation
:- op(550,xfy,imp).
:- op(500,yfx,@+).		    % sum (nat)
:- op(500,yfx,#+).		    % sum (int)
:- op(500,yfx,or).
:- op(500,yfx,#-).		    % subtraction (int)
:- op(400,yfx,@*).		    % product (nat)
:- op(400,yfx,and).
:- op(400,yfx,#*).		    % product (int)
:- op(300,fy,#-).		    % minus (int)
:- op(300,fy,neg).
:- op(100,fy,?).		    % variables: ?x, ?y, ?z, ?v, ?0, ?1, ?2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_fofs(+File)
%   - File.pl is a file name of a Prolog program P, 
%     File.pr is an LPTP proof file about the Prolog code in File.pl
%  Produces as many files Files<i>.fof as there are lemmas in File.pr.

filename_fofs(File):-
    filename_nbLemmas(File,N),
    write(File), write('.pr contains '),write(N), writeln(' lemmas.'),
    filename_fofs_(1,N,File).
    
    filename_fofs_(P,N,_) :- P > N, !.
    filename_fofs_(I,N,F) :- I =< N, J is I +1,
        atom_concat(F,I,FI),atom_concat(FI,'.fof',FIfof),
        write('Creating file '),write(FIfof),write(' ... '),
        create_fof_file(F,FIfof,I),writeln(' done.'),
        filename_fofs_(J,N,F).
    
create_fof_file(File,FIfof,Indice) :-
    setup_call_cleanup(
        open(FIfof,write,Stream),
        filename_indice_stream(File,Indice,Stream),
        close(Stream)),
    !.
create_fof_file(_File,FIfof,Indice) :-
    % Never leave a half-written .fof behind: it parses as a plausible problem
    % file while missing its conjecture, so a prover would just report it as
    % satisfiable instead of failing loudly.
    format(user_error,
           "*** ERROR: could not generate ~w (lemma ~w); removing partial file.~n",
           [FIfof,Indice]),
    catch(delete_file(FIfof),_,true),
    fail.

/*
filename_fofs('member/member').
filename_fofs('add_peano/add_peano').
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_fofs_run(+File):
%   idem filename_fofs, but
%   applies Vampire and Eprover on each file
%   compares and summarizes the result

filename_fofs_run(File):-
    filename_nbLemmas(File,N),
    shell('rm tmp_outputE; touch tmp_outputE'),
    shell('rm tmp_outputV; touch tmp_outputV'),
    write(N), writeln(' lemmas'),
    filename_fofs_(1,N,File,0/SuccessV,0/SuccessE),
    write(N), writeln(' lemmas, '),
    report_ratio('Vampire',SuccessV,N),
    report_ratio('Eprover',SuccessE,N).

    % report_ratio(+Prover,+Success,+N): guarded against N =:= 0
    report_ratio(Prover,Success,N) :-
        write(Success), write(' proved by '), write(Prover),
        (   N > 0
        ->  Ratio is (Success*100)/N, write(', ratio: '), writeln(Ratio)
        ;   writeln(', ratio: n/a (no lemma)')
        ).

    filename_fofs_(P,N,_,Sv/Sv,Se/Se) :- P > N, !.
    filename_fofs_(I,N,F,Sv0/Sv,Se0/Se) :- I =< N, J is I +1,
        atom_concat(F,I,FI),atom_concat(FI,'.fof',FIfof),
        write('creating file '),writeln(FIfof),
        create_fof_file(F,FIfof,I),
        %
        atom_concat('vampire --proof off --time_limit 1s ',FIfof,CmdV0),
        atom_concat(CmdV0, ' >> tmp_outputV',CmdV),
        shell(CmdV,StatusV),
        write('status vampire: '),writeln(StatusV),
        %
        atom_concat('eprover --auto --cpu-limit=1 -s ',FIfof,CmdE0),
        %atom_concat('vampire --proof off --time_limit 1s ',FIfof,CmdE0),
        atom_concat(CmdE0, ' >> tmp_outputE',CmdE),
        shell(CmdE,StatusE),
        write('status eprover: '),writeln(StatusE),
        %
        ((StatusV==0,StatusE\==0) -> (write('**** for file '), write(FI),
            writeln(', Vampire > Eprover')); true),
        ((StatusV\==0,StatusE==0) -> (write('**** for file '), write(FI),
            writeln(', Eprover > Vampire')); true),
        %
        (StatusV == 0 -> Sv1 is Sv0+1 ; Sv1 is Sv0),
        (StatusE == 0 -> Se1 is Se0+1 ; Se1 is Se0),
        filename_fofs_(J,N,F,Sv1/Sv,Se1/Se).
    
/*
filename_fofs_run('member/member').
filename_fofs_run('add_peano/add_peano').
% 
filename_fofs_run('even_odd/even_odd').
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_nbLemmas(+File,-N):
%   - File.pr is an LPTP proof file
%   - N is the number of lemmas/corollaries/theorems in File.pr

filename_nbLemmas(File,N) :-
    filename_lemmas_defns_axs(File,AllLemmas,_,_),
    length(AllLemmas,N).

/*
filename_nbLemmas('member/member',N).
filename_nbLemmas('nat/nat',N).
filename_nbLemmas('list/list',N).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_indice_stream(+File,+I,+Stream):
%   - File
%   - I is an integer >= 1 and =< NbLemmas in File.pr
%   - Stream is the stream where the fof description will be written
%   All lemmas the index of which are < Index are considered as axioms.
%   Definition of functions and predicates from the proof file File.pr
%   are also considered as axioms.
%   The Ith lemma is a conjecture, which may have associated induction axioms.

filename_indice_stream(File,Indice,_Stream) :-
    filename_nbLemmas(File,N), Indice > N, !,
    writeln('Error: index > #lemmas'), fail.
filename_indice_stream(File,Indice,Stream) :-
    Indice > 0,
    reset_gensym(id),   % fof names restart at id1 in every generated file
    filename_ax1_8(File,Axioms1_to_8),
    write_ax1_ax8_as_TPTP_axioms(Axioms1_to_8,Stream),
    filename_indice_defs_lemmas_induction_conjecture(File,Indice,Defs,Axs,Lemmas,InductionAxioms,Conjecture),
    write_axioms_as_TPTP_axioms(Stream,Axs),
    write_defns_as_TPTP_axioms(Stream,Defs),
    write_lemmas_as_TPTP_axioms(Stream,Lemmas),
    write_lemmas_as_TPTP_axioms(Stream,InductionAxioms),
    write_lemma_as_TPTP_conjecture(Stream,Conjecture).

write_ax1_ax8_as_TPTP_axioms([],_S).
write_ax1_ax8_as_TPTP_axioms([Ax|Axs],S) :-
    write(S,'fof('),gensym(id,Id),write(S,Id),write(S,',axiom,'),
    write_LPTP_formula_to_TPTP(S,Ax),writeln(S,').'),
    write_ax1_ax8_as_TPTP_axioms(Axs,S).
    
/*
between(1,4,I),filename_indice_stream('add_peano/add_peano',I,user).
*/
 
/*
:- axiom(<name>,<LPTP_formula>).   
*/
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_defs_lemmas_induction_conjecture(+File,+Indice,-Defs,-Lemmas,-InductionAxioms,-Conjecture):
%   - File.pl is a Prolog file, File.pr an LPTP proof file about File.pl
%   - Defs is a list of defined functions and predicates appearing in File.pr
%   - Lemmas is the list of lemmas/corollaries/theorems in File.pr, in the same order
%   - Induction Axioms is a list of induction axioms for the Ith lemma (Axiom 10 of Ind(P))
%   - Conjecture is the Ith lemma of File.pr

filename_indice_defs_lemmas_induction_conjecture(File,Indice,Defs,Axs,Lemmas,InductionAxioms,Conjecture) :-
    filename_completion(File,Comp), % Comp in LPTP syntax
    filename_lemmas_defns_axs(File,AllLemmas,Defs,Axs), % AllLemmas and Defs in LPTP syntax
    length(AllLemmas,N), 
    0 < Indice, Indice =< N, J is Indice-1,
    length(Lemmas,J),
    append(Lemmas,[Conjecture|_],AllLemmas),
    Conjecture = lemma(Name,FConjecture0),
    name_fconjecture_comp_induction(Name,FConjecture0,Comp,InductionAxioms).
    
    % name_fconjecture_comp_induction(+Name,+FConjecture0,+Comp,-InductionAxioms) 
    % FConjecture0 and Comp in LPTP syntax
    name_fconjecture_comp_induction(Name,FConjecture0,Comp,InductionAxioms) :-
        FConjecture0 = (all _Vars:(_L => _R)), !,
        Lemma = lemma(Name,FConjecture0),
        induction_for_lemma(Lemma,Comp,Closed,Sub),
        InductionAxioms = [lemma(induction,(Closed => Sub))].
    name_fconjecture_comp_induction(Name,FConjecture0,Comp,InductionAxioms) :-
        % a lemma without variables on a 0-ary predicate p,
        % e.g. succeeds p => fail or succeeds p <=> fail:
        % treat it as all [] : F; quantify_all/3 drops the empty prefix
        ( FConjecture0 = (L => R) ; FConjecture0 = (L <=> R) ),
        zero_ary_induction_atom(L => R), !,
        name_fconjecture_comp_induction(Name,(all [] : FConjecture0),Comp,InductionAxioms).
    name_fconjecture_comp_induction(Name,FConjecture0,Comp,InductionAxioms) :-
        FConjecture0 = (all Vars:(L <=> R)), !,
        FConjectures = ((all Vars : L => R) & (all Vars : R => L)),
        name_fconjecture_comp_induction(Name,FConjectures,Comp,InductionAxioms).
    name_fconjecture_comp_induction(Name,FConjectures,Comp,InductionAxioms) :-
        % mutual induction for 2 predicates
        FConjectures = (FConjecturesL & FConjecturesR), !,
        name_fconjecture_comp_induction(Name,FConjecturesL,Comp,IAL),
        name_fconjecture_comp_induction(Name,FConjecturesR,Comp,IAR),
        append(IAL,IAR,InductionAxioms).
    name_fconjecture_comp_induction(_Name,_FConjecture0,_Comp,InductionAxioms) :-
        InductionAxioms = [].

    % zero_ary_induction_atom(+Implication): the atom on which induction_for_lemma/4
    % would induct for Implication (see split_lemma/3) is 0-ary
    zero_ary_induction_atom(Implication) :-
        split_lemma((all [] : Implication), _-Atom, _),
        atom(Atom).
		
/*
filename_indice_defs_lemmas_induction_conjecture('member/member',1,Defs,Lemmas,InductionAxioms,Conjecture),writeln(InductionAxioms).
filename_indice_defs_lemmas_induction_conjecture('even_odd/even_odd',1,Defs,Lemmas,InductionAxioms,Conjecture),writeln(InductionAxioms).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_lemmas_defns(+F,-L,-D):
%   - F.pr is an LPTP proof file name 
%   - L is the list of lemmas (corollaries/theorems) appearing in F, in the same order,
%       represented as lemma(Id,F) in LPTP syntax
%   - D is the list of definitions (fn/pred) appearing in F, in the same order
%       represented as def(Name,Arity,F) in LPTP syntax

/* Syntax of LPTP lemmas and an example:

:- lemma(Name:Name, Formula, Proof).

:- lemma(nat:termination, all x: succeeds nat(?x) => terminates nat(?x), ... ).

We ignore the proof.
This lemma is translated in fof syntax as:
        fof('nat:termination', axiom, ! [X] : (nat_succeeds(X) => nat_terminates(X))).
or      fof('nat:termination', conjecture, ! [X] : (nat_succeeds(X) => nat_terminates(X))).
by the write_lemma_as_TPTP_conjecture/2 predicate, see below.

Syntax and examples of LPTP definitions:

:- definition_fun(Name,Arity,Formula,Existence,Uniqueness).
:- definition_pred(Name,Arity,Formula).

:- definition_fun(@+,2,
 all [x,y,z]: succeeds nat(?x) => 
  (?x @+ ?y = ?z <=> succeeds plus(?x,?y,?z)),
 existence by lemma(plus:existence),
 uniqueness by lemma(plus:uniqueness)).

To be written as:
    fof('@+/2', axiom, ! [X,Y,Z] : (nat_succeeds(X) => (X @+ Y = Z <=> plus_succeeds(X,Y,Z)))).

:- definition_pred(sub,2,
all [l1,l2]: sub(?l1,?l2) <=>
    (all x: succeeds member(?x,?l1) => succeeds member(?x,?l2))).

To be written as:
    fof('sub/2', axiom, ! [L1,L2] : (sub(L1,L2) <=> (all X: (member_succeeds(X,L1) => member_succeeds(X,L2))))).
*/


filename_lemmas_defns_axs(File,Lms,Defns,Axs) :-
    atom_concat(File,'.pr',FilePr),
    setup_call_cleanup(
        open(FilePr,read,Stream),
        ( read(Stream,T),
          filename_lemmas_defns_axs(T,Stream,[],Lms,[],Defns,[],Axs) ),
        close(Stream)).

    filename_lemmas_defns_axs(end_of_file,_,Lms,Lemmas,Dfs,Defns,Axs,Axioms) :- !, 
        reverse(Lms,Lemmas),
        reverse(Dfs,Defns),
        reverse(Axs,Axioms).
    filename_lemmas_defns_axs(( :- axiom(Id,F)),S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- !, 
        read(S,T), 
        filename_lemmas_defns_axs(T,S,Lms0,Lms,Dfs0,Dfs,[axiom(axiom-Id,F)|Axs],Axioms).
    filename_lemmas_defns_axs(( :- lemma(Id,F,_)),S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- !, 
        read(S,T), 
        filename_lemmas_defns_axs(T,S,[lemma(lemma-Id,F)|Lms0],Lms,Dfs0,Dfs,Axs,Axioms).
    filename_lemmas_defns_axs(( :- theorem(Id,F,_)),S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- !, 
        read(S,T), 
        filename_lemmas_defns_axs(T,S,[lemma(theorem-Id,F)|Lms0],Lms,Dfs0,Dfs,Axs,Axioms).
    filename_lemmas_defns_axs(( :- corollary(Id,F,_)),S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- !, 
        read(S,T), 
        filename_lemmas_defns_axs(T,S,[lemma(corollary-Id,F)|Lms0],Lms,Dfs0,Dfs,Axs,Axioms).
    filename_lemmas_defns_axs(( :- definition_fun(N,A,F,_E,_U)),S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- !,
        read(S,T),
        filename_lemmas_defns_axs(T,S,Lms0,Lms,[def(N,A,F)|Dfs0],Dfs,Axs,Axioms).
    filename_lemmas_defns_axs(( :- definition_pred(N,A,F)),S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- !,
        read(S,T),
        filename_lemmas_defns_axs(T,S,Lms0,Lms,[def(N,A,F)|Dfs0],Dfs,Axs,Axioms).
    filename_lemmas_defns_axs(_,S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms) :- 
        read(S,T), 
        filename_lemmas_defns_axs(T,S,Lms0,Lms,Dfs0,Dfs,Axs,Axioms).

/*
filename_lemmas_defns_axs('add_peano/add_peano',L,D,A),write_list(L),write_list(D),write_list(A),fail.
filename_lemmas_defns_axs('nat_min/nat_min',L,D,A),write_list(L),write_list(D),write_list(A),fail.
filename_lemmas_defns_axs('nat/nat',L,D,A),write_list(L),write_list(D),write_list(A),fail.
filename_lemmas_defns_axs('list_min/list_min',L,D,A),write_list(L),write_list(D),write_list(A),fail.
filename_lemmas_defns_axs('list/list',L,D,A),write_list(L),write_list(D),write_list(A),fail.
*/    
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
% write_lemma_as_TPTP_conjecture(+Stream,+Lemma)
% write_lemmas_as_TPTP_axioms(Stream,+Axioms)
% write_defns_as_TPTP_axioms(+Stream,+Defns)
% write_axioms_as_TPTP_axioms(+Stream,+Axs)

write_lemma_as_TPTP_conjecture(S,lemma(N,F)) :-    
    write(S,'fof('),term_to_atom(N,T),write_canonical(S,T),write(S,',conjecture,'),
    write_LPTP_formula_to_TPTP(S,F),writeln(S,').').

write_lemmas_as_TPTP_axioms(S,L) :- write_lemmas_to_fof_axioms(S,L).   

    write_lemmas_to_fof_axioms(_,[]):- !.
    write_lemmas_to_fof_axioms(S,[lemma(N,F)|Ls]) :-
        write(S,'fof('),term_to_atom(N,T),write_canonical(S,T),write(S,',axiom,'),
        write_LPTP_formula_to_TPTP(S,F),writeln(S,').'),
        write_lemmas_to_fof_axioms(S,Ls).
                     
write_defns_as_TPTP_axioms(S,L) :- write_defs_to_fof_axioms(S,L).   

    write_defs_to_fof_axioms(_,[]) :- !.
    write_defs_to_fof_axioms(S,[def(N,A,F)|Axs]) :-
        write(S,'fof('),term_to_atom(N/A,T),write_canonical(S,T),write(S,',axiom,'),
        write_LPTP_formula_to_TPTP(S,F),writeln(S,').'),
        write_defs_to_fof_axioms(S,Axs).

write_axioms_as_TPTP_axioms(Stream,Axs) :- write_axioms_to_fof_axioms(Stream,Axs).

    write_axioms_to_fof_axioms(_,[]):- !.
    write_axioms_to_fof_axioms(S,[axiom(N,F)|Ls]) :-
        write(S,'fof('),term_to_atom(N,T),write_canonical(S,T),write(S,',axiom,'),
        write_LPTP_formula_to_TPTP(S,F),writeln(S,').'),
        write_axioms_to_fof_axioms(S,Ls).

    

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
% write_LPTP_formula_to_TPTP(+Stream,+LPTPFormula):
%   write LPTPFormula in fof syntax onto Stream
% See the definition of LPTP formula p. 40 of the LPTP user manual

write_LPTP_formula_to_TPTP(S,LPTPFormula) :- write_(S,LPTPFormula). 

        write_(S,true) :- !, write(S,'$true'). 
        write_(S,tt) :- !, write(S,'$true').   
        write_(S,ff) :- !, write(S,'$false'). 
        write_(S,fail) :- !, write(S,'$false').         

        write_(S,L = R) :- !, write__term(S,L), write(S,' = '), write__term(S,R).
        write_(S,L <> R) :- !, write_(S, ~ (L = R) ).   
        write_(S,L #< R) :- !,write_quoted_functor(S,'#<'),write(S,'('),write__term(S,L),write(S,','),write__term(S,R),write(S,')').
        write_(S,L #=< R) :- !,write_quoted_functor(S,'#=<'),write(S,'('),write__term(S,L),write(S,','),write__term(S,R),write(S,')').

        write_(S,gr(T)) :- !, write(S,'gr('), write__term(S,T), write(S,')').
        write_(S,~ F) :- !, write(S,'~ ('),write_(S,F),write(S,')').
        write_(S,F & G) :- !, write(S,'('),write_(S,F),write(S,' & '),write_(S,G),write(S,')').    
        write_(S,F \/ G) :- !, write(S,'('),write_(S,F),write(S,' | '),write_(S,G),write(S,')').    
        write_(S,F => G) :- !, write(S,'('),write_(S,F),write(S,' => '),write_(S,G),write(S,')').    
        write_(S,F <=> G) :- !, write(S,'('),write_(S,F),write(S,' <=> '),write_(S,G),write(S,')').    
        % An empty quantifier prefix is no prefix at all: TPTP has no '! [] : F'.
        write_(S,all [] : F) :- !, write_(S,F).
        write_(S,ex [] : F) :- !, write_(S,F).
        write_(S,all [X|Xs] : F) :- !, write(S,'! ['),  write_variables(S,[X|Xs]), write(S,'] : '),write_(S,F).
        write_(S,all Id : F) :- !, write(S,'! ['), write__term(S,?(Id)), write(S,'] : '),write_(S,F).
        write_(S,ex [X|Xs] : F) :- !, write(S,'? ['),  write_variables(S,[X|Xs]), write(S,'] : '),write_(S,F).
        write_(S,ex Id : F) :- !, write(S,'? ['), write__term(S,?(Id)), write(S,'] : '),write_(S,F).
        write_(S,F) :- atomic(F), !, write_functor(S,F).
        % A 0-ary predicate must be written 'p_succeeds', never 'p_succeeds()':
        % the latter is a TPTP syntax error, and write_terms/2 used to just fail
        % on the empty list, truncating the file in the middle of a formula.
        write_(S,succeeds(At)) :- !,
            At =.. [P|Args], atom_concat(P,'_succeeds',Ps), write_functor(S,Ps),
            write_args(S,Args).
        write_(S,fails(At)) :- !,
            At =.. [P|Args], atom_concat(P,'_fails',Ps), write_functor(S,Ps),
            write_args(S,Args).
        write_(S,terminates(At)) :- !,
            At =.. [P|Args], atom_concat(P,'_terminates',Ps), write_functor(S,Ps),
            write_args(S,Args).
        write_(S,Atom) :-
            Atom =.. [P|Args], write_functor(S,P), write_args(S,Args).

            write_args(_,[]) :- !.
            write_args(S,[A|As]) :- write(S,'('), write_terms(S,[A|As]), write(S,')').

            write_terms(_,[]) :- !.
            write_terms(S,[T]) :- !, write__term(S,T).
            write_terms(S,[T1,T2|Us]) :- write__term(S,T1), write(S,','),write_terms(S,[T2|Us]).

            % write_quoted_functor(+Stream,+Functor): emit 'F' with the quotes,
            % as TPTP requires for non-alphanumeric functors. Written character by
            % character so that the code stays valid under --traditional / ISO,
            % where "..." is a code list rather than a string object.
            write_quoted_functor(S,F) :- write(S,''''), write(S,F), write(S,'''').

            % write_functor(+Stream,+Functor): quote unless the functor is a plain
            % TPTP identifier. write_canonical/2 is NOT enough here: it leaves a
            % purely symbolic atom such as '@=<' or '#<' unquoted, which is legal
            % Prolog but a TPTP syntax error -- E stops on "just read '@'".
            write_functor(S,F) :-
                (   tptp_plain_functor(F)
                ->  write(S,F)
                ;   write_quoted_functor(S,F)
                ).

                tptp_plain_functor(F) :-
                    atom(F),
                    atom_codes(F,[C|Cs]),
                    C >= 0'a, C =< 0'z,
                    forall(member(D,Cs), tptp_alnum(D)).

                    tptp_alnum(D) :- D >= 0'a, D =< 0'z.
                    tptp_alnum(D) :- D >= 0'A, D =< 0'Z.
                    tptp_alnum(D) :- D >= 0'0, D =< 0'9.
                    tptp_alnum(0'_).

            %write__term(S,[]) :- !, term_to_atom([],At), write_canonical(S,At).
            write__term(S,[]) :- !, write(S,nil).
            write__term(S,Cst) :- integer(Cst), term_to_atom(Cst,QuotedInt),!, write_functor(S,QuotedInt).
            write__term(S,Cst) :- atomic(Cst), !, write_functor(S,Cst).
            write__term(S,?(Id)) :-  !, atom_concat('X',Id,A),write(S,A).
            write__term(S,#- X) :- !, write_quoted_functor(S,'#-'),write(S,'('),write__term(S,X),write(S,')').
            write__term(S,-(X)) :- !, write_quoted_functor(S,'-'),write(S,'('),write__term(S,X),write(S,')').

            write__term(S,[X|Xs]) :- !, write(S,'cons('),write__term(S,X),write(S,','),write__term(S,Xs),write(S,')').
            write__term(S,X ** Y) :- !, write_quoted_functor(S,'**'),write(S,'('),write__term(S,X),write(S,','),write__term(S,Y),write(S,')').
            write__term(S,X @+ Y) :- !, write_quoted_functor(S,'@+'),write(S,'('),write__term(S,X),write(S,','),write__term(S,Y),write(S,')').
            write__term(S,X @* Y) :- !, write_quoted_functor(S,'@*'),write(S,'('),write__term(S,X),write(S,','),write__term(S,Y),write(S,')').
            write__term(S,X #+ Y) :- !, write_quoted_functor(S,'#+'),write(S,'('),write__term(S,X),write(S,','),write__term(S,Y),write(S,')').
            write__term(S,X #* Y) :- !, write_quoted_functor(S,'#*'),write(S,'('),write__term(S,X),write(S,','),write__term(S,Y),write(S,')').
            write__term(S,X #- Y) :- !, write_quoted_functor(S,'#-'),write(S,'('),write__term(S,X),write(S,','),write__term(S,Y),write(S,')').

            write__term(S,Term) :-
                Term =.. [F|Args], write_functor(S,F),
                write(S,'('), write_terms(S,Args),write(S,')').
            
             write_variables(S,[X]) :- !,write__term(S,?(X)).
             write_variables(S,[X,Y|Zs]) :- write__term(S,?(X)), write(S,','), write_variables(S,[Y|Zs]).

/*
filename_lemmas_defns_axs('add_peano/add_peano',L,_,_),member(C,L),write(user,'%%'),write_lemma_as_TPTP_conjecture(user,C),fail.
filename_lemmas_defns_axs('nat_min',L,_,_),member(C,L),write(user,'%%'),write_lemma_as_TPTP_conjecture(user,C),fail.
filename_lemmas_defns_axs('nat',L,D,_),member(C,L),write(user,'%%'),write_lemma_as_TPTP_conjecture(user,C),fail.
filename_lemmas_defns_axs('list_min',L,_,_),member(C,L),write(user,'%%'),write_lemma_as_TPTP_conjecture(user,C),fail.      
filename_lemmas_defns_axs('list',L,_,_),member(C,L),write(user,'%%'),write_lemma_as_TPTP_conjecture(user,C),fail.          
 
filename_lemmas_defns_axs('add_peano/add_peano',L,D,_),write_lemmas_as_TPTP_axioms(user,L),write_defns_as_TPTP_axioms(user,D),fail.
filename_lemmas_defns_axs('nat_min',L,D,_),write_lemmas_as_TPTP_axioms(user,L),write_defns_as_TPTP_axioms(user,D),fail.
filename_lemmas_defns_axs('nat',L,D,_),write_lemmas_as_TPTP_axioms(user,L),write_defns_as_TPTP_axioms(user,D),fail.            
filename_lemmas_defns_axs('list_min',L,D,_),write_lemmas_as_TPTP_axioms(user,L),write_defns_as_TPTP_axioms(user,D),fail.            
filename_lemmas_defns_axs('list',L,D,_),write_lemmas_as_TPTP_axioms(user,L),write_defns_as_TPTP_axioms(user,D),fail.            

filename_lemmas_defns_axs('add_peano/add_peano',L,_,_),append(L1,[C],L),write_lemmas_as_TPTP_axioms(user,L1),write_lemma_as_TPTP_conjecture(user,C),fail.
*/
            
            

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% induction_for_lemma(+Lemma,+Completion,-Closed,-Sub):
/*
- for a lemma of the form:  all [x,y,...] : (((success p(?x) & F) & G) => H)
    - rewrite it as:  all [x]: (success p(?x) => (all [y,...] : F & G => H))
    - let Phi(?x) :=  (all [y,...] : F & G => H))
      (so the lemma is:  all[x] : (success p(?x) => Phi(?x))
    - generate the induction axiom:
        Closed:= [ all [x] : [ S D_p(?x) => Phi(?x)] ] where on the left
            each "success p(t)" is replaced by "success p(t) & Phi(t)" 
            (NB : correctly quantify the variables of term t)
        Sub := [all [x] : (success p(?x) => Phi(?x)) ]   
        (NB : it is the lemma to be proved)
        Induction Axiom :=  [Closed => Sub]

- for a 0-ary predicate p, the same scheme applies with p in place of p(?x),
  hence without the prefix all [x] (Phi may still quantify over [y,...]):
    - the completion of p is (p <=> D_p), without quantifier,
      see definition/4;
    - a lemma without any variable, e.g. (success p => fail), is first
      rewritten as all [] : (success p => fail),
      see name_fconjecture_comp_induction/4;
    - Closed and Sub get no quantifier prefix (see quantify_all/3):
        Closed := [S D_p => Phi], where on the left
            each "success p" is replaced by "success p & Phi"
        Sub := [success p => Phi]
      e.g. for the clause (p :- q, p) and the lemma (success p => fail),
      so that Phi = fail:
        Closed := [(success q & (success p & fail)) => fail]
        Sub := [success p => fail]
*/

% induction_for_lemma(+Lemma,+Completion, -Closed, -Sub)
% Lemma and Completion in LPTP syntax
induction_for_lemma(Lemma, Completion, Closed, Sub) :-
    Lemma = lemma(_Id,FLemma),
    split_lemma(FLemma, AtomVars-Atom, RightVars-Right),
    % The cut must come AFTER definition/4: a lemma may well mention a predicate
    % that has no completion here (one imported through needs_gr/needs_thm).
    % Cutting before it commits to this clause and then fails, which silently
    % aborts the whole .fof file instead of falling back to the trivial axiom.
    definition(Completion, Atom, AtomDef, Definition),
    !,
    (RightVars = [] ->
        Phi = Right
    ;
        Phi = (all RightVars : Right) ),
    quantify_all(AtomVars, (succeeds Atom => Phi), Sub),
    renaming(AtomDef,Atom,Renaming),
    apply_renaming(Definition,Renaming,DefinitionR),
    success(DefinitionR, SuccessDef),
    success(Atom,Atom_succeeds),
    add_property(SuccessDef, Atom_succeeds, Phi, SDP),
    quantify_all(AtomVars, (SDP => Phi), Closed).
induction_for_lemma(_Lemma, _Completion, true, true).

    % quantify_all(+Vars,+Formula,-QuantifiedFormula): a 0-ary predicate yields
    % Vars = [], and 'all [] : F' is neither an LPTP nor a TPTP formula.
    quantify_all([], F, F) :- !.
    quantify_all([V|Vs], F, (all [V|Vs] : F)).
      
    split_lemma( (all Vars : (succeeds Atom => Right)), AtomVars-Atom, RightVars-Right) :- !,
        Atom =.. [_|AVars], lptp_variables_id(AVars,AtomVars),
        findall(Var,(member(Var,Vars),\+ member(?(Var),AVars)),RightVars).    
    split_lemma( (all Vars : ((As & A) => Right)), AtomVars-Atom, RightVars-Phi) :- 
        first_atom((As & A),Atom,Remainder),
        Phi = (Remainder => Right),
        Atom =.. [_|AVars], lptp_variables_id(AVars,AtomVars),
        findall(Var,(member(Var,Vars),\+ member(?(Var),AVars)),RightVars).
    
        first_atom((succeeds A) & B, A, B) :- !.
        first_atom((As & A), SAtom, (A & Remainder)) :- !,
            first_atom(As, SAtom, Remainder).

            lptp_variables_id([],[]).
            lptp_variables_id([?(Y)|Xs],[Y|Ys]) :-  lptp_variables_id(Xs,Ys).

    % a completion is quantified iff its predicate has arity N > 0
    definition([all _:(Atom <=> Def)|_], Atom0, Atom, Def) :- functor(Atom,P,N), N> 0, functor(Atom0,P,N), !.
    definition([(Atom <=> Def)|_], Atom0, Atom, Def) :- functor(Atom,P,0), functor(Atom0,P,0), !.
    definition([_|L], Atom, AtomDef, Def) :- definition(L, Atom, AtomDef, Def).

    %add_property(+SuccessDef,+Atom,+Phi,-SDP)
    add_property(true, _Atom, _Phi, true) :- !.
    add_property(fail, _Atom, _Phi, fail) :- !.
    add_property(S = T, _Atom, _Phi, S = T) :- !.
    add_property((~ G), _Atom, _Phi, (~ G)) :- !.
    add_property((F & G), Atom, Phi, (F2 & G2)) :- !,add_property(F,Atom,Phi,F2), add_property(G,Atom,Phi,G2).
    add_property((F \/ G), Atom, Phi, (F2 \/ G2)) :- !,add_property(F,Atom,Phi,F2), add_property(G,Atom,Phi,G2).
    add_property((ex V : F), Atom, Phi, (ex V : F2)) :- !, add_property(F, Atom, Phi, F2).
    add_property(Atom, At, _Phi, Atom) :- functor(Atom,P,N), functor(At,Q,M), dif(P/N,Q/M), !.
    add_property(Atom, At, Phi, (At3 & Phi3)) :- functor(Atom,P,N), functor(At,P,N), 
        % NB : At is always a pure atom pred(?x,?y,...)
        copy_term_lptp(At-Phi,At2-Phi2),
        renaming(At2,Atom,Subst),
        apply_renaming(At2,Subst,At3),
        apply_renaming(Phi2,Subst,Phi3).
      
/*
add_property(member_succeeds(?a, ?b), member_succeeds(?x, ?l), gr(?l)=>gr(?x), F).		
*/		
		
/*
induction_for_lemma(lemma(1, all[x, l]:succeeds member(?x, ?l)&gr(?l)=>gr(?x)), 
    [all[?x1,?x2]:member(?x1, ?x2)<=>(ex[?x3]:ex[?x4]: ?x2=cons(?x3, ?x4)&member(?x1, ?x4))\/(ex[?x5]: ?x2=cons(?x1, ?x5))], Closed,Sub).

% p :- p.
induction_for_lemma(lemma(1, all[]:succeeds p => fail),
    [p <=> p], Closed,Sub).
% p_succeeds&fail=>fail
% succeeds p=>fail

% p :- q, p.   q.
induction_for_lemma(lemma(2, all[]:succeeds p => fail),
    [p <=> q & p, q <=> true], Closed,Sub).
% q_succeeds&(p_succeeds&fail)=>fail
% succeeds p=>fail

% p :- q, p.   q.   (0-ary atom, but Phi quantifies over x)
induction_for_lemma(lemma(3, all[x]:succeeds p => ?x = 0),
    [p <=> q & p, q <=> true], Closed,Sub).
% q_succeeds&(p_succeeds&(all[x]: ?x=0))=>(all[x]: ?x=0)
% succeeds p=>(all[x]: ?x=0)

% p :- q ; p.   q.
induction_for_lemma(lemma(4, all[]:succeeds p => succeeds q),
    [p <=> q \/ p, q <=> true], Closed,Sub).
% q_succeeds\/p_succeeds&succeeds q=>succeeds q
% succeeds p=>succeeds q

% r :- n(s(0)).   n(0).   n(s(X)) :- n(X).   (0-ary r defined by an n-ary n)
induction_for_lemma(lemma(5, all[]:succeeds r => succeeds n(0)),
    [r <=> n(s(0)), all[x1]:n(?x1)<=>(ex[x2]: ?x1=s(?x2)&n(?x2))\/ ?x1=0], Closed,Sub).
% n_succeeds(s(0))=>succeeds n(0)
% succeeds r=>succeeds n(0)
*/
		
		
		
% renaming(+Pure_Atom1,+Pure_Atom2,-Subst)
% with vars(Pure_Atom1) disjoint from vars(Pure_Atom2)           
renaming(Pure_Atom1,Pure_Atom2,Subst) :- 
    functor(Pure_Atom1,P,N), functor(Pure_Atom2,P,N),
    renaming(1,N,Pure_Atom1,Pure_Atom2,Subst).
            
    renaming(P,N,_,_,[]) :- P > N, !.
    renaming(I,N,T1,T2,[A1-A2|S]) :- 
        I =< N, J is I+1,
        arg(I,T1,?(A1)), arg(I,T2,A2),
        renaming(J,N,T1,T2,S).

% ?- renaming(p(?x,?y),p(?a,?b),R).
% R = [x- ?a, y- ?b].


% copy_term_lptp(+PureLPTPAtom-Form,-RenamedPureLPTPAtom-RenamedForm)
%   The fresh names MUST avoid every identifier already occurring in the pair,
%   the quantified ones included. gensym(y,...) alone is not enough: LPTP proof
%   files routinely quantify over y1, y2, ..., and when a generated name hits one
%   of them apply_renaming/4 treats it as bound and silently captures it, which
%   corrupts the induction hypothesis (seen on nat.pr, lemma plus:leq:leq).
copy_term_lptp(PureLPTPAtom-Form,RenamedPureLPTPAtom-RenamedForm) :-
    PureLPTPAtom =.. [_|Args],
    lptp_names(PureLPTPAtom-Form,Used),
    build_renaming(Args,Used,Renaming),
    apply_renaming(PureLPTPAtom,Renaming,RenamedPureLPTPAtom),
    apply_renaming(Form,Renaming,RenamedForm).

build_renaming([],_Used,[]).
build_renaming([?(X)|LPTPVars],Used,[X - ?(GS)|Renaming]) :-
    fresh_name(Used,GS),
    build_renaming(LPTPVars,[GS|Used],Renaming).

    fresh_name(Used,GS) :- repeat, gensym(y,GS), \+ memberchk(GS,Used), !.

% lptp_names(+Term,-Names): the identifiers occurring in Term. Over-approximated
% by every atom in it -- cheap, and only ever used to steer clear of collisions.
lptp_names(Term,Names) :-
    findall(A, (lptp_subterm(Term,S), atom(S), A = S), As),
    sort(As,Names).

    lptp_subterm(T,T).
    lptp_subterm(T,S) :- compound(T), arg(_,T,A), lptp_subterm(A,S).


% ?- copy_term_lptp(nat_succeeds(?x)-gr(?x), A-F).
% A = nat_succeeds(?y17), F = gr(?y17).

% copy_term_lptp(p(?x,?y,?z)-phi(?y,?z), A-F).
% copy_term_lptp(p(?x,?y,?z)- (ex [y]:phi(?x,?y,?z)), A-F).
% copy_term_lptp(p(?x,?y,?z)- (ex y:phi(?x,?y,?z)), A-F).
% copy_term_lptp(p(?x,?y,?z)- (all [x] : (ex [y]:phi(?x,?y,?z))), A-F).
% copy_term_lptp(p(?x,?y,?z)- (all [x] : (ex [y]: (all z:phi(?x,?y,?z)))), A-F).

           
% apply_renaming(+LPTPFormula,+Renaming,-LPTPRenamedFormula) 
apply_renaming(LPTPFormula, Renaming, LPTPRenamedFormula) :-
    apply_renaming(LPTPFormula, Renaming, [], LPTPRenamedFormula).

% apply_renaming(+LPTPFormula, +Renaming, +QuantifiedVariables, -LPTPRenamedFormula)
apply_renaming(true, _R, _QV, true) :- !.
apply_renaming(fail, _R, _QV, fail) :- !.  
apply_renaming(S = T, R, QV, Rs = Rt) :- !, apply_renaming_term(S, R, QV, Rs), apply_renaming_term(T, R, QV, Rt).
apply_renaming((~ F), R, QV, (~ Rf)) :- !, apply_renaming(F, R, QV, Rf).
apply_renaming((F & G), R, QV, (Rf & Rg)) :- !, apply_renaming(F, R, QV, Rf), apply_renaming(G, R, QV, Rg).
apply_renaming((F \/ G), R, QV, (Rf \/ Rg)) :- !, apply_renaming(F, R, QV, Rf), apply_renaming(G, R, QV, Rg).
apply_renaming((ex [] : F), R, QV, Rf) :- !, apply_renaming(F, R, QV, Rf).
apply_renaming((all [] : F), R, QV, Rf) :- !, apply_renaming(F, R, QV, Rf).
apply_renaming((ex [V|Vs] : F), R, QV, (ex [V|Vs] : Rf)) :- !, append([V|Vs], QV, QV2), apply_renaming(F, R, QV2, Rf).
apply_renaming((ex V : F), R, QV, (ex [V] : Rf)) :- atom(V), !, apply_renaming(F, R, [V|QV], Rf).
apply_renaming((all [V|Vs] : F), R, QV, (all [V|Vs] : Rf)) :- !, append([V|Vs], QV, QV2), apply_renaming(F, R, QV2, Rf).
apply_renaming((all V : F), R, QV, (all [V] : Rf)) :- atom(V), !, apply_renaming(F, R, [V|QV], Rf).
apply_renaming(Atom, R, QV, Ratom) :-
    Atom =.. [P|Args], length(Args, N), length(Rargs, N), Ratom =.. [P|Rargs],
    apply_renaming_terms(Args, R, QV, Rargs).

    apply_renaming_terms([], _R, _QV, []).
    apply_renaming_terms([T|Ts], R, QV, [Rt|Rts]) :- 
        apply_renaming_term(T, R, QV, Rt), 
        apply_renaming_terms(Ts, R, QV, Rts).
                
        apply_renaming_term(C, _R, _QV, C) :- atomic(C), !.
        apply_renaming_term(?(Id), _R, QV, ?(Id)) :- member(Id, QV), !.        
        apply_renaming_term(?(Id), R, _QV, GS) :- member(Id-GS, R), !.
        apply_renaming_term(?(Id), _R, _QV, ?(Id)) :- !.
        apply_renaming_term(Term, R, QV, Rterm) :-
            Term =.. [F|Args], length(Args, N), length(Rargs, N), Rterm =.. [F|Rargs],
            apply_renaming_terms(Args, R, QV, Rargs).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_completion(+File,-Comp)
%   - File.pl is a Prolog file P
%   - Comp is the completion of P, as a list of LPTP formulas

filename_completion(File,Comp) :-
    atom_concat(File,'.pl',FilePl),
    filename_clauses(FilePl,Cls),
    ground_completion(Cls,Comp,_,_,_).
    

defs_ground_variables([],[]).
defs_ground_variables([D|Ds],[CleanDef|CleanDefs]) :- 
    term_variables(D,Vars),
    reset_gensym(x),   % NOT reset_gensym/0: that also resets the 'id' and 'y' counters
    ground_vars(Vars),
    clean_defn(D,CleanDef),
    defs_ground_variables(Ds,CleanDefs).

    
        
ground_vars([]).
ground_vars([X|Xs]) :- gensym(x,Idx), X = ?(Idx), ground_vars(Xs).

% [all[_A]:p(_A)<=>_A=a\/(ex[X]:_A=c&(p(X)&q(c))),  all[_B]:q(_B)<=>_B=b],
clean_defn(all Vars : (Head <=> Def), all CVars: (Head <=> CDef)) :- !, clean_vars(Vars,CVars), clean_body_def(Def,CDef).
clean_defn((Head <=> Def), (Head <=> CDef)) :-  clean_body_def(Def,CDef).

clean_vars([],[]).
clean_vars([?(Id)|IMIds],[Id|CIMIds]) :- clean_vars(IMIds,CIMIds).

clean_body_def(true,true):- !.
clean_body_def(false,fail):- !.
clean_body_def(L = R,L = R):- !.
clean_body_def(A & B, CA & CB) :- !, clean_body_def(A,CA), clean_body_def(B,CB).
clean_body_def(A \/ B, CA \/ CB) :- !, clean_body_def(A,CA), clean_body_def(B,CB).
clean_body_def((ex Vars : F), (ex CVars : CF)) :- !, clean_vars(Vars,CVars), clean_body_def(F,CF).
clean_body_def(At,At).


    
/*
filename_completion('member/member',Comp).
filename_completion('member/member',Comp),write_list(user,Comp),fail.
filename_completion('nat/nat',Comp),write_list(user,Comp),fail.
filename_completion('list/list',Comp),write_list(user,Comp),fail.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Borrowed from Simply Logical, Peter Flach, 
% https://book.simply-logical.space/src/simply-logical.html
% with various bugs correction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
/*
The main task in Predicate Completion is the completion of each separate predicate definition. 
The main steps are:
    (i) adding explicit unifications to the body of clauses;
    (ii) adding existential quantifiers for those variables occurring in the body 
         of a clause but not in its head;
    (iii) combining the clauses into one formula, and adding universal quantifiers
          for the head variables.
The predicate unifications_and_quantifiers/2 takes care of the first two steps,
and the third step is carried out by the predicate complete_formula/3. 
*/
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% completion(+P,-Comp,-DefPIs,-UndefPIs,-FIs): 
%       - P is a logic program represented as a list of (H :- [B|Bs]) or (H :- [])
%       - Comp the completion of P, as a list of (pseudo)-LPTP formulas
%         NB: P and F use non-ground representation.
%       - DefPIs is a list of predicate indicators P/N defined in P 
%       - UndefPIs is a list of predicate indicators P/N used but undefined in P 
%       - FIs is a list of function indicators used in P
%
% Example for F: the (pseudo)-LPTP for the definition of member/2
% all[_A, _B]:member(_A, _B) <=>
%   (ex[_C]:ex[_D]:_B=cons(_C, _D)&member(_A, _D))
%       \/
%   (ex[_E]:_B=cons(_A, _E))

completion([],[],[],[],[]).
completion([C|Cs],Comp,DefPIs,UndefPIs,FNs) :-
    program_function_symbols([C|Cs],FNs),
    separate_definitions([C|Cs],Definitions), 
    complete_definitions(Definitions,CompDefs,Heads), 
    defined_pred(Heads,DefPIs),
    handle_undefined([C|Cs],Heads,CompDefs,Comp,UndefPIs).

ground_completion(Cls,GrdComp,DefPIs,UndefPIs,FNs):-
    completion(Cls,Comp,DefPIs,UndefPIs,FNs),
    defs_ground_variables(Comp,GrdComp).

defined_pred([],[]).
defined_pred([H|Hs],[P/N|PIs]):- functor(H,P,N), defined_pred(Hs,PIs).

separate_definitions([],[]). 
separate_definitions([(H :- B)|Cls],[[(H :- B)|D]|Ds]) :-
    get_definition(Cls,H,D,Rest), 
    separate_definitions(Rest,Ds).
    
get_definition([],_Head,[],[]). 
get_definition([(H :- B)|Cls],Head,[(H :- B)|Def],Rest) :- 
    same_predicate(H,Head), !,
    get_definition(Cls,Head,Def,Rest). 
get_definition([(H :- B)|Cls],Head,Def,[(H :- B)|Rest]) :-
    \+ same_predicate(H,Head), 
    get_definition(Cls,Head,Def,Rest).

same_predicate(H1,H2) :- functor(H1,P,N), functor(H2,P,N).


% Undefined predicates are those which occur in bodies of clauses without occurring in any head. 
% The list Heads of defined predicates is obtained while completing each predicate definition. 
% Care must be taken to avoid considering not/1 as an undefined predicate, 
% and also to check the negated literal itself. 
% After constructing the list of undefined literals occuring in clause bodies, 
% each of them is transformed into a formula of the form !X1...!Xn: ¬p(X1,...,Xn)

handle_undefined(Program,Heads,CompDefs,Comp,UndefPIs) :-
    findall(P/N,
            ( member((_H :- B),Program),           % pick a clause body,
              body_literal(B,L),                   % any literal in it, at any depth
              \+ member(L,Heads),                  % which is undefined
              functor(L,P,N), dif(P,=) ),          % and is not equality
            PIs),
    sort(PIs,UndefPIs),                            % one axiom per predicate, not per call
    undef_formulas(UndefPIs,CompDefs,Comp).

undef_formulas([],Comp,Comp).
undef_formulas([PI|PIs],Comp0,Comp) :-
    quantify(PI,F),
    undef_formulas(PIs,[F|Comp0],Comp).

% quantify(+PredicateIndicator,-Formula)
quantify(P/0,(P <=> false)) :- !.
quantify(P/N,F) :-
    N > 0,
    length(Vars,N),
    NewL =.. [P|Vars],
    F = ( all Vars: (NewL <=> false)).
 

% complete_definitions(D,C,H): 
%       C is the complement of definitions D, and H is  list of variablised heads 
complete_definitions([Def],[Comp],[Head]) :- !, 
    complete_definition(Def,Comp,Head). 
complete_definitions([Def|Defs],[Comp | Comps],[H|Hs]) :-
    complete_definition(Def,Comp,H), 
    complete_definitions(Defs,Comps,Hs).

complete_definition(Definition,Comp,Head) :- 
    unifications_and_quantifiers(Definition,F), 
    complete_formula(F,Comp,Head).
    
unifications_and_quantifiers([],[]). 
unifications_and_quantifiers([Clause|Clauses],[C|Cs]) :-
    unifs_and_quants(Clause,C), 
    unifications_and_quantifiers(Clauses,Cs).

unifs_and_quants((Head :- Body),(NewHead :- NewBody)) :- 
    Head=..[Pred|Args], 
    explicit_unifications(Args,NewArgs,Body,TmpBody), 
    existential_quantifiers(TmpBody,NewArgs,NewBody), 
    NewHead=..[Pred|NewArgs].
    
% explicit_unifications(A,NA,B,NB):
%       NA is list A with non-var. terms replaced by new var.s; 
%       NB is body B extended with explicit unifications 

% explicit_unifications(Args,NewArgs,Body,TmpBody)

explicit_unifications(Args,NewArgs,Body,TmpBody) :-
    explicit_unifications_(Args,NewArgs,[],Body,TmpBody).
    
explicit_unifications_([],[],_,Body,Body). 
explicit_unifications_([T|As],[V|NewAs],Vars,B,[V=T|NewB]) :-
    nonvar(T), !, % add explicit unification
    explicit_unifications_(As,NewAs,Vars,B,NewB). 
explicit_unifications_([Var|As],NewAs0,SeenVars,Body,NewBody0) :-
    var(Var),  
    (var_element(Var,SeenVars) ->
        NewAs0 = [W|NewAs], NewBody0 = [W=Var|NewBody], SeenVars0 = SeenVars
        ;
        NewAs0 = [Var|NewAs], NewBody0 = NewBody, SeenVars0 = [Var|SeenVars]
    ),
    explicit_unifications_(As,NewAs,SeenVars0,Body,NewBody).
    

% existential_quantifiers(B,V,NB):
%       NB is conj. of lit.s in B, 
%       extended by ex. quant.s for var.s in B but not in V 

existential_quantifiers(Body,HeadVars,NewBody) :-
    varsin(Body,BodyVars), % built-in predicate 
    body_form(Body,Conj), % list -> conjunction
    body_quants(BodyVars,HeadVars,Conj,NewBody).

% varsin(Term,Vs) <- Vs is a list of the variables in Term
varsin(Term,Vs) :- term_variables(Term,Vs).

body_form([], true).
body_form([G], F) :- !, goal_form(G,F).
body_form([G|List], F & Conj) :- goal_form(G,F), body_form(List,Conj).

% goal_form(+PrologGoal,-LPTPFormula):
%   translate one body element into an LPTP goal formula.
%   Control constructs follow LPTP's own translation, see lptp/src/gnd.pl,
%   gnd__disjunction/5 and gnd__goal/4:
%       ( C -> T ; E )  ==>  (C & T) \/ (~ C & E)
%       ( C -> T )      ==>  (C & T)                 [ i.e. ( C -> T ; fail ) ]
%       ( A ; B )       ==>  A \/ B
%       \+ A , not A    ==>  ~ A
%   Once here, the completion is a pure LPTP formula, so gr/2, success/2,
%   failure/2 and termination/2 need no if-then-else clause of their own.

goal_form(G,F) :- var(G), !, F = G.
goal_form(\+ A, (~ Fa)) :- !, goal_form(A,Fa).
goal_form(not A, (~ Fa)) :- !, goal_form(A,Fa).
goal_form((C -> T ; E), ((Fc & Ft) \/ ((~ Fc) & Fe))) :- !,
    goal_form(C,Fc), goal_form(T,Ft), goal_form(E,Fe).
goal_form((C -> T), (Fc & Ft)) :- !, goal_form(C,Fc), goal_form(T,Ft).
goal_form((A ; B), (Fa \/ Fb)) :- !, goal_form(A,Fa), goal_form(B,Fb).
goal_form((A , B), (Fa & Fb)) :- !, goal_form(A,Fa), goal_form(B,Fb).
goal_form(true, true) :- !.
goal_form(fail, fail) :- !.
goal_form(false, fail) :- !.
goal_form(Atom, Atom).

% control_goal(+Goal): Goal is a control construct, not a callable atom.
control_goal(G) :- nonvar(G), control_goal_(G).

    control_goal_((_ -> _ ; _)).
    control_goal_((_ -> _)).
    control_goal_((_ ; _)).
    control_goal_((_ , _)).
    control_goal_((\+ _)).
    control_goal_((not _)).

% control_subgoals(+ControlGoal,-ImmediateSubgoals)
control_subgoals((C -> T ; E),[C,T,E]) :- !.
control_subgoals((C -> T),[C,T]) :- !.
control_subgoals((A ; B),[A,B]) :- !.
control_subgoals((A , B),[A,B]) :- !.
control_subgoals((\+ A),[A]) :- !.
control_subgoals((not A),[A]) :- !.

% control_atom_goal(+Goal): a goal that denotes no predicate of the program.
control_atom_goal(true).
control_atom_goal(fail).
control_atom_goal(false).

% body_literal(+BodyGoalList,-Literal):
%   enumerate, on backtracking, the callable literals of a clause body,
%   descending through negations and control constructs.
body_literal(Body,L) :- member(G,Body), goal_literal(G,L).

    goal_literal(G,_) :- var(G), !, fail.
    goal_literal(G,L) :- control_goal(G), !,
        control_subgoals(G,Gs), member(G2,Gs), goal_literal(G2,L).
    goal_literal(G,_) :- control_atom_goal(G), !, fail.
    goal_literal(L,L).

% body_quants(BV,HV,C,QC):
%       QC is conj. C extended with existential quant.s for all
%       variables in BV but not in HV 

body_quants([],_HeadVars,Conj,Conj). 
body_quants([BVar|BVars],HeadVars,Conj,( ex [BVar] : F)) :-
    \+ var_element(BVar,HeadVars), !,
    body_quants(BVars,HeadVars,Conj,F).
body_quants([BVar|BVars],HeadVars,Conj,F) :- 
    var_element(BVar,HeadVars), 
    body_quants(BVars,HeadVars,Conj,F).

var_element(X,[Y|_Ys]) :- X == Y, !.  % syntactic identity
var_element(X,[_Y|Ys]) :- var_element(X,Ys).


% complete_formula(C,F,H):
%       F is disjunction of bodies of clauses in C, and univ. quantified head H 

complete_formula(C,Formula,Head) :-
    combine_clauses(C,Head,Body), 
    varsin(Head,HeadVars), 
    head_quants(HeadVars,(Head <=> Body),Formula).
    
combine_clauses([(Head :- Body)],Head,Body) :- !. 
combine_clauses([(Head :- Body)|R],Head,(Body \/ RBody)):-
    combine_clauses(R,Head,RBody).

head_quants([],Formula,Formula). 
head_quants([HVar|HVars],Formula, (all [HVar|HVars] : Formula)).



% program_function_symbols(P,FNs):
%       FNs is a list of the function symbols of P
    
program_function_symbols(P,FNs) :-
    program_fns(P,[],FNs).
        
program_fns([],FNs,FNs).
program_fns([C|Cs],FNs0,FNs) :-
    C = (H :- Body),
    atom_fnsyms(H,FNs0,FNs1),
    body_fnsyms(Body,FNs1,FNs2),
    program_fns(Cs,FNs2,FNs).

atom_fnsyms(H,Fs,Fs) :-
    var(H), !.
atom_fnsyms(H,Fs0,Fs) :-
    control_goal(H), !,                 % descend into \+, ',', ';' and if-then-else
    control_subgoals(H,Gs),
    body_fnsyms(Gs,Fs0,Fs).
atom_fnsyms(H,Fs,Fs) :-
    control_atom_goal(H), !.            % true/fail/false denote no function symbol
atom_fnsyms(H,Fs0,Fs) :-
    H =.. [_P|Args],
    terms_fnsyms(Args,Fs0,Fs).
    
  terms_fnsyms([],Fs,Fs).
  terms_fnsyms([A|As],Fs0,Fs) :-
      term_fnsyms(A,Fs0,Fs1),
      terms_fnsyms(As,Fs1,Fs).
    
  term_fnsyms(X,Fs,Fs) :- var(X), !.
  term_fnsyms(T,Fs0,Fs) :-
      nonvar(T),
      functor(T,F,N),
      add_fs(Fs0,F/N,Fs1), 
      T =.. [F|Args],
      terms_fnsyms(Args,Fs1,Fs).
    
  add_fs(Fs0,F/N,Fs) :- 
      (member(F/N,Fs0) -> Fs = Fs0 ; Fs = [F/N|Fs0]).

body_fnsyms([],FNs,FNs).
body_fnsyms([A|As],FNs0,FNs) :-
    atom_fnsyms(A,FNs0,FNs1),
    body_fnsyms(As,FNs1,FNs).    
    
/*    
The following queries illustrate the operation of the program: 

P = [], ground_completion(P,F,DPIs,UPIs,FNs).    
P = [(p :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p :- [p])],ground_completion(P,F,DPIs,UPIs,FNs).    
P = [(p :- [q])],ground_completion(P,F,DPIs,UPIs,FNs).    
P = [(p(a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(X) :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(s(X)) :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(X,a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(X,Y) :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(X,X) :- [])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(a) :- []),(q(b) :- [])], ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(a) :- []),(q(b) :- []),(p(c) :- [p(X),q(c)])], ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p(s(X)) :- [p(X)]),(p(0) :- [])], ground_completion(P,F,DPIs,UPIs,FNs), writeln(F).
P = [(p :- [q]),(q :- [])],ground_completion(P,F,PDPIs,UPIsIs,FNs).
P = [(p :- [\+ q])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [(p :- [q(X)])],ground_completion(P,F,DPIs,UPIs,FNs).
P = [member(A,[B|C]):-[member(A,C)]],ground_completion(P,F,DPIs,UPIs,FNs),write(F),nl.
P = [(member(X,[X|_]) :- []),(member(A,[B|C]):-[member(A,C)])],ground_completion(P,F,DPIs,UPIs,FNs),write(F),nl.
*/
    
/*                
filename_clauses('member/member.pl',L),ground_completion(L,F,PIs,UPIs,FNs),write_list(L),write_list(F).
filename_clauses('nat/nat.pl',L),ground_completion(L,F,PIs,UPIs,FNs),write_list(L),write_list(F).
filename_clauses('list/list.pl',L),ground_completion(L,F,PIs,UPIs,FNs),write_list(L),write_list(F).
*/

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gr(+LPTPin,-LPTPout)
% success(+LPTPin,-LPTPout)
% failure(+LPTPin,-LPTPout)
% termination(+LPTPin,-LPTPout)
    
%%%%%
gr(true, true):-!.
gr(fail, true):-!.
gr(S = T, gr(S) & gr(T)):-!.
gr((G & H), (GrG & GrH)) :- !, gr(G,GrG), gr(H,GrH).
gr((G \/ H), (GrG & GrH)) :- !, gr(G,GrG), gr(H,GrH).
gr((ex V :G), (ex V :GrG)):- !, gr(G,GrG).
gr( (~ G), GrG) :- !, gr(G,GrG).
gr(Atom, GrA) :- Atom =.. [_|Args], grs(Args,GrA).

  grs([],true).
  grs([A],gr(A)) :- !.
  grs([A1,A2|As],gr(A1) & GrAs) :- grs([A2|As],GrAs).

%%%%%
success(true,true) :- !.
success(fail, fail) :- !.
success(S = T, S = T) :- !.
success((~ G), FG) :- !, failure(G,FG).
success((G & H), (SG & SH)) :- !, success(G,SG), success(H,SH).
success((G \/ H), (SG \/ SH)) :- !, success(G,SG), success(H,SH).
success((ex V :G),(ex V :SG)) :- !, success(G,SG).
success(Atom, SAt) :- 
    Atom =.. [P|Args],dif(P,=),
    atom_concat(P,'_succeeds',PS),
    SAt =.. [PS|Args].

%%%%%
failure(true, fail) :- !.
failure(fail, true) :- !.
failure(S = T, (~ (S = T))) :- !.
failure((~ G), SG) :- !, success(G,SG).
failure((G & H), (FG \/ FH)) :- !, failure(G,FG), failure(H,FH).
failure((G \/ H), (FG & FH)) :- !, failure(G,FG), failure(H,FH).
failure((ex V :G),(all V :FG)) :- !, failure(G,FG).
failure(Atom, FAt) :- 
    Atom =.. [P|Args], dif(P,=),
    atom_concat(P,'_fails',PF),
    FAt =.. [PF|Args].

%%%%%    
termination(true,true) :- !.
termination(fail, true) :- !.
termination(_ = _, true) :- !.
termination((~ G), (TG & GrG)) :- !, termination(G,TG), gr(G,GrG).
termination((G & H), (TG & (FG \/ TH))) :- !, termination(G,TG), failure(G,FG), termination(H,TH).
termination((G \/ H), (TG & TH)) :- !, termination(G,TG), termination(H,TH).
termination((ex V:G),(all V:TG)) :- !, termination(G,TG).
termination(Atom, TAt) :- 
    Atom =.. [P|Args],dif(P,=),
    atom_concat(P,'_terminates',PT),
    TAt =.. [PT|Args].
    
/*
G = ( ex [x2]: (?x4 = [?x0|?x2])),gr(G,Gr).  
Gr = (ex[x2]:gr(?x4)&gr([?x0|?x2])).

G = (ex [x6]: ex [x8]:(?x4=[?x6|?x8] & member(?x0,?x8)) ),gr(G,GrG).
GrG = (ex[x6]:ex[x8]:gr(?x4)&gr([?x6|?x8])&(gr(?x0)&gr(?x8))).

G = (( ex [x2]: (?x4 = [?x0|?x2])) \/ (ex [x6]: ex [x8]:(?x4=[?x6|?x8] & member(?x0,?x8)) )),gr(G,GrG).
GrG = ((ex[x2]:gr(?x4)&gr([?x0|?x2]))&(ex[x6]:ex[x8]:gr(?x4)&gr([?x6|?x8])&(gr(?x0)&gr(?x8)))).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_ax1_ax8(+File,-Axioms1_to_8):
%   - File.pl is a file name of a Prolog program P
%   - Axioms1_to_8 is a list of the axioms 1-8 of IND(P) (without axiom 3) in fof syntax
 
filename_ax1_8(File,Axioms1_to_8) :-
    atom_concat(File,'.pl',FilePl),
    filename_clauses(FilePl,Cls),
    ground_completion(Cls,Comp,DefPIs,UndefPIs,FIs),
    axioms1and2(FIs, Ax12),
    axioms4and5(FIs, Ax45),
    axioms6and7(DefPIs,UndefPIs,Ax67),
    axioms8(DefPIs,UndefPIs,Comp,Ax8),
    append([Ax12,Ax45,Ax67,Ax8],Axioms1_to_8).

% ground_vars(+Vars)
% clean_vars(+?Ids,-Ids)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% axioms1and2(+FIs,-Ax12):
%   - FIs is a list of functor/arity
%   - Ax12 is a list of axioms for Clark's equality theory in LPTP syntax
%
% axioms1and2([s/1,0/0,+/2],L).    

axioms1and2([],[]).
axioms1and2([F/0|FIs],Ax) :-
    !, 
    axioms2(FIs,F,0,Ax0),
    append(Ax0,Ax1,Ax),
    axioms1and2(FIs,Ax1).
axioms1and2([F/N|FIs],Ax) :-
    N>0, 
    axioms1(N,F,N,Ax0),
    axioms2(FIs,F,N,Ax1),
    append(Ax0,Ax1,Ax2),
    append(Ax2,Ax3,Ax),
    axioms1and2(FIs,Ax3).
    
    axioms1(0,_F,_N,[]) :- !.
    axioms1(I,F,N,[Ax|Axioms]) :-
        I > 0, J is I -1,
        length(XVars,N), nth1(I,XVars,Xi), Term1 =.. [F|XVars],
        length(YVars,N), nth1(I,YVars,Yi), Term2 =.. [F|YVars],
        ground_vars(XVars),clean_vars(XVars,Xs),
        ground_vars(YVars),clean_vars(YVars,Ys),
        Ax = (all Xs : ( all Ys : ( Term1 = Term2 => Xi = Yi))),
        axioms1(J,F,N,Axioms).
        
    axioms2([],_F,_N,[]).
    axioms2([G/0|FIs],F,0,[Ax|Axioms]) :-
        !, 
        Ax = (~ (F = G)),
        axioms2(FIs,F,0,Axioms).
    axioms2([G/N|FIs],F,0,[Ax|Axioms]) :-
        N > 0, !, 
        length(Vars,N), Term2 =.. [G|Vars],
        ground_vars(Vars),clean_vars(Vars,Xs),
        Ax = (all Xs : ~ (F = Term2)),
        axioms2(FIs,F,0,Axioms).
    axioms2([G/0|FIs],F,M,[Ax|Axioms]) :-
        M > 0, !, 
        length(Vars,M), Term2 =.. [F|Vars],
        ground_vars(Vars),clean_vars(Vars,Xs),
        Ax = (all Xs : ~ (G = Term2)),
        axioms2(FIs,F,M,Axioms).        
    axioms2([G/N|FIs],F,M,[Ax|Axioms]) :-
        N > 0, M > 0, 
        length(XVars,M), Term1 =.. [F|XVars],
        length(YVars,N), Term2 =.. [G|YVars],
        ground_vars(XVars),clean_vars(XVars,Xs),
        ground_vars(YVars),clean_vars(YVars,Ys),
        Ax = (all Xs : (all Ys : ~ (Term1 = Term2))),
        axioms2(FIs,F,M,Axioms).
        
        
  
 /*   
P = [], ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12).
P = [(p :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p :- [p])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p :- [q])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(X) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(s(X)) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(X,a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(b,a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(X,Y) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(X,X) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(a) :- []),(q(b) :- [])], ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p(a) :- []),(q(b) :- []),(p(c) :- [])], ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(nat(s(X)) :- [nat(X)]),(nat(0) :- [])], ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p :- [q]),(q :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p :- [\+ q])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p :- [q(X)])],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [member(A,[B|C]):-[member(A,C)]],ground_completion(P,F,DPIs,UPIs,FNs),axioms1and2(FNs,Ax12). 
P = [(p([]) :- []),(member(X,[X|_]) :- []),(member(A,[B|C]):-[member(A,C)])],completion(P,F,DPIs,UPIs,FNs),
       axioms1and2(FNs,Ax12). 
*/        
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% axioms4and5(+FIs,-Ax45):
%   - FIs is a list of functor/arity
%   - Ax45 is a list of axioms for gr/1 in FOF syntax

axioms4and5([], []).
axioms4and5([F/0|FIs], [gr(F)|Axioms]) :-
    !, 
    axioms4and5(FIs,Axioms).
axioms4and5([F/N|FIs],[Ax|Axioms]) :-
    N > 0,
    length(Vars, N),
    Term =.. [F|Vars],
    ground_vars(Vars),clean_vars(Vars,Xs),
    grs(Vars,Grs),
    Ax = ( all Xs : (Grs <=> gr(Term))),
    axioms4and5(FIs,Axioms).

/*   
P = [], completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p :- [p])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p :- [q])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(a) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(X) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(s(X)) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(X,a) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(X,Y) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(X,X) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p(a) :- []),(q(b) :- [])], completion(P,F,DPIs,UPIs,FNs),a,axioms4and5(FNs,Axioms).
P = [(p(a) :- []),(q(b) :- []),(p(c) :- [])], completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(nat(s(X)) :- [nat(X)]),(nat(0) :- [])], completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p :- [q]),(q :- [])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p :- [\+ q])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(p :- [q(X)])],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [member(A,[B|C]):-[member(A,C)]],completion(P,F,DPIs,UPIs,FNs),axioms4and5(FNs,Axioms).
P = [(member(X,[X|_]) :- []),(member(A,[B|C]):-[member(A,C)])],completion(P,F,DPIs,UPIs,FNs),
        axioms4and5(FNs,Axioms).
*/
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% axioms6and7(+DefPIs,+UndefPIs,-Ax67):
%   - DefPIs is a list of predicate/arity for defined predicates
%   - UndefPIs is a list of predicate/arity for undefined predicates
%   - Ax67 is a list of axioms 6 and 7 in FOF syntax

axioms6and7(DefPIs,UndefPIs,Ax67) :-
    append(DefPIs,UndefPIs,PIs),
    axioms6and7(PIs,Ax67).
    
    axioms6and7([], []).
    axioms6and7([P/0|PIs], [Ax6,Ax7|Ax]) :-
        !,
        atom_concat(P,'_succeeds',PS), 
        atom_concat(P,'_fails',PF), 
        atom_concat(P,'_terminates',PT), 
        Ax6 = ~(PS & PF),
        % '|' is not the LPTP disjunction (it would be written out as an
        % uninterpreted predicate '|'/2), and the disjunct is PF, not PS again.
        % Compare with the N > 0 clause below.
        Ax7 =  (PT => (PS \/ PF)),
        axioms6and7(PIs, Ax).
    axioms6and7([P/N|PIs], [Ax6,Ax7|Ax]) :-
        N > 0,
        length(Vars,N),
        atom_concat(P,'_succeeds',PS), Rs =.. [PS|Vars],
        atom_concat(P,'_fails',PF), Rf =.. [PF|Vars],
        atom_concat(P,'_terminates',PT), Rt =.. [PT|Vars],
        ground_vars(Vars),clean_vars(Vars,Xs),
        Ax6 = (all Xs : ~(Rs & Rf)),
        Ax7 = (all Xs : (Rt => (Rs \/ Rf))),
        axioms6and7(PIs, Ax).

  
 /*   
P = [], completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p :- [p])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p :- [q])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(a) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(X) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(s(X)) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(X,a) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(X,Y) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(X,X) :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(a) :- []),(q(b) :- [])], completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p(a) :- []),(q(b) :- []),(p(c) :- [])], completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(nat(s(X)) :- [nat(X)]),(nat(0) :- [])], completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p :- [q]),(q :- [])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p :- [\+ q])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(p :- [q(X)])],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [member(A,[B|C]):-[member(A,C)]],completion(P,F,DPIs,UPIs,FNs),axioms6and7(DPIs,UPIs,Ax67). 
P = [(member(X,[X|_]) :- []),(member(A,[B|C]):-[member(A,C)])],completion(P,F,DPIs,UPIs,FNs),
       axioms6and7(DPIs,UPIs,Ax67). 
*/
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% axioms8(+DefPIs,+UndefPIs,+Comp,-Ax8):
%   - DefPIs is a list of predicate/arity for defined predicates
%   - UndefPIs is a list of predicate/arity for undefined predicates
%   - Comp is the completion of the Prolog program, in FOF syntax
%   - Ax8 is a list of axioms 8 in FOF syntax

axioms8(DPIs,UPIs,Comp,Ax8) :-
    axioms8_def(DPIs,Comp,Ax8_Def),
    axioms8_undef(UPIs,Ax8_Undef),
    append(Ax8_Def,Ax8_Undef,Ax8).


    axioms8_undef([], []).
    axioms8_undef([P/0|PIs], [SuccessP,FailureP,TerminationP|Ax8]) :-
        !,
        atom_concat(P,'_succeeds',PS), 
        atom_concat(P,'_fails',PF), 
        atom_concat(P,'_terminates',PT), 
        SuccessP = (~ PS), 
        FailureP = PF,
        TerminationP = PT,
        axioms8_undef(PIs,Ax8).
    axioms8_undef([P/N|PIs], [SuccessP,FailureP,TerminationP|Ax8]) :-
        N > 0, 
        atom_concat(P,'_succeeds',PS), length(Argss,N),  As =.. [PS|Argss],
        atom_concat(P,'_fails',PF), length(Argsf,N), Af =.. [PF|Argsf],
        atom_concat(P,'_terminates',PT), length(Argst,N),  At =.. [PT|Argst],
        %
        ground_vars(Argss),clean_vars(Argss,Xs),
        SuccessP = (all Xs : (~ As)), 
        %
        ground_vars(Argsf),clean_vars(Argsf,Ys),
        FailureP = (all Ys : Af),
        %
        ground_vars(Argst),clean_vars(Argst,Zs),
        TerminationP = (all Zs : At),
        %
        axioms8_undef(PIs,Ax8).
   
        
    axioms8_def([],_,[]).
    axioms8_def([P/0|PIs],Comp, [SuccessP,FailureP,TerminationP|Ax8]) :-
        member((P <=> DP),Comp), !,
        atom_concat(P,'_succeeds',PS), 
        atom_concat(P,'_fails',PF), 
        atom_concat(P,'_terminates',PT), 
        success(DP,SDP), SuccessP = (PS <=> SDP), 
        failure(DP,FDP), FailureP = (PF <=> FDP),
        termination(DP,TDP), TerminationP = (PT <=> TDP),
        axioms8_def(PIs,Comp,Ax8).
    axioms8_def([P/N|PIs],Comp, [SuccessP,FailureP,TerminationP|Ax8]) :-
        N > 0, %lptp_variables_id([?(Y)|Xs],[Y|Ys]) 
        length(Args,N), lptp_variables_id(VArgs,Args),Atom =.. [P|VArgs],
        member( (all Args : (Atom <=> DP)), Comp), !, 
        atom_concat(P,'_succeeds',PS), As =.. [PS|VArgs],
        atom_concat(P,'_fails',PF), Af =.. [PF|VArgs],
        atom_concat(P,'_terminates',PT), At =.. [PT|VArgs],
        %ground_vars(Args),clean_vars(Args,Xs),
        success(DP,SDP), SuccessP = (all Args : (As <=> SDP)), 
        failure(DP,FDP), FailureP = (all Args : (Af <=> FDP)), 
        termination(DP,TDP), TerminationP = (all Args : (At <=> TDP)), 
        axioms8_def(PIs,Comp,Ax8).


/*
P = [], ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p :- [p])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).   
P = [(p :- [q])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).  
P = [(p(a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(X) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(s(X)) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(X,a) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(X,Y) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(X,X) :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(a) :- []),(q(b) :- [])], ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p(a) :- []),(q(b) :- []),(p(c) :- [])], ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(nat(s(X)) :- [nat(X)]),(nat(0) :- [])], ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8),
        write_list(Ax8).
P = [(p :- [q]),(q :- [])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p :- [\+ q])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8).
P = [(p :- [q(X)])],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8),write_list(Ax8).
P = [member(A,[B|C]):-[member(A,C)]],ground_completion(P,F,DPIs,UPIs,FNs),axioms8(DPIs,UPIs,F,Ax8),write_list(Ax8).
P = [(member(X,[X|_]) :- []),(member(A,[B|C]):-[member(A,C)])],ground_completion(P,F,DPIs,UPIs,FNs),
        axioms8(DPIs,UPIs,F,Ax8),write_list(Ax8).
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write_list(L):
% write_list(Stream,L):
%   write one element (terminated by a '.' per line

write_list([]).
write_list([X|Xs]) :- write(X), write('.'), nl, write_list(Xs).

write_list(_,[]) :- !.
write_list(S,[X|Xs]) :-
    \+ \+ (numbervars(X,0,_N),write(S,X),write(S,'.'),nl),
    write_list(S,Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filename_clauses(+F,-L):
%   - F is a Prolog file name 
%   - L  a corresponding list of clauses (H :- [B|Bs]) or (H :- [])
%   Directives are ignored.
%   Non-ground representation.

filename_clauses(File,Cls) :-
    setup_call_cleanup(
        open(File,read,Stream),
        ( read(Stream,T),
          filename_clauses_aux(T,Stream,[],Cls) ),
        close(Stream)).

    filename_clauses_aux(end_of_file,_,Cls,Cls) :- !.
    filename_clauses_aux(T,Stream,Cls0,Cls) :- 
        prologClause_Cl(T,Cl),
        rewrite_preds(Cl,Cls0,Cls1),
        read(Stream,T2),
        filename_clauses_aux(T2,Stream,Cls1,Cls).
            
    prologClause_Cl(( :- B), directive(SqB)) :- !,rnd_sq(B,SqB).
    prologClause_Cl( (H :- B), (H :- SqB)) :- !,rnd_sq(B,SqB).
    prologClause_Cl(H, (H :- [])).

    rnd_sq((not(B),Br),[(\+ B)|Bs]) :- !, rnd_sq(Br,Bs).
    rnd_sq((B,Br),[B|Bs]) :- !, rnd_sq(Br,Bs).
    rnd_sq(not(B),[(\+ B)]) :- !.   % 'not' as the last body literal, too
    rnd_sq(At,[At]).
    
    rewrite_preds(directive(_),Cls,Cls).
    rewrite_preds((H :- B),Cls,[(RwH :- RwB)|Cls]) :-
        rewrite_pred(H,RwH),
        rewrite_body(B,RwB).

        rewrite_body([],[]).
        rewrite_body([A|As],[B|Bs]) :- rewrite_pred(A,B), rewrite_body(As,Bs).

        rewrite_pred(G,G) :- var(G), !.
        rewrite_pred(G,G2) :-
            control_goal(G), !,                 % rewrite the branches as goals,
            rewrite_control(G,G2).              % not as terms
        rewrite_pred(G,G) :- control_atom_goal(G), !.
        rewrite_pred(Atom,Atom2) :-
            Atom =.. [P|Args],
            term_to_atom(P,PAt),
            rewrite_terms(Args,Args2),
            Atom2 =.. [PAt|Args2].

            rewrite_control((C -> T ; E),(C2 -> T2 ; E2)) :- !,
                rewrite_pred(C,C2), rewrite_pred(T,T2), rewrite_pred(E,E2).
            rewrite_control((C -> T),(C2 -> T2)) :- !,
                rewrite_pred(C,C2), rewrite_pred(T,T2).
            rewrite_control((A ; B),(A2 ; B2)) :- !,
                rewrite_pred(A,A2), rewrite_pred(B,B2).
            rewrite_control((A , B),(A2 , B2)) :- !,
                rewrite_pred(A,A2), rewrite_pred(B,B2).
            rewrite_control((\+ A),(\+ A2)) :- !, rewrite_pred(A,A2).
            rewrite_control((not A),(\+ A2)) :- !, rewrite_pred(A,A2).
                
            rewrite_term(X,X) :- var(X),!.
            rewrite_term([],nil) :- !.
            rewrite_term(X,At) :- dif(X,[]), atomic(X), !, term_to_atom(X, At).
            rewrite_term([X|Xs],cons(X2,X2s)) :- !, rewrite_term(X,X2), rewrite_term(Xs,X2s).
            rewrite_term(T,T2) :- T =.. [F|Args], rewrite_terms(Args,Args2), T2 =.. [F|Args2].
            
                rewrite_terms([],[]).
                rewrite_terms([T|Ts],[T2|T2s]) :- rewrite_term(T,T2), rewrite_terms(Ts,T2s).
        
/*
filename_clauses('member/member.pl',L),write_list(user,L).
filename_clauses('list/list.pl',L),write_list(user,L).
filename_clauses('nat/nat.pl',L),write_list(user,L).
*/