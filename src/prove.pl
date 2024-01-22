:- include('./indp').

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
:- op(600,yfx,doubleslash).% application of substitutions
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
% filename_fofs_run_single_prover(+File):
%   idem filename_fofs_run, but
%   applies single prover to each .fof file (first-order form)
%   before summarizing the result
%   instead of applying provers (Vampire and Eprover)
%   to each file

filename_fofs_run_single_prover(File):-
    filename_nbLemmas(File,N),

    truncate_output_file,

    write(N), writeln(' lemmas'),
    filename_fofs_single(1,N,File,0/SuccessProver),

    write(N), write(' lemmas, '),
    write(SuccessProver), write(' proved, '),
    ProverRatio is (SuccessProver*100)/N, write(' ratio: '), writeln(ProverRatio),
    halt.

    filename_fofs_single(P,N,_,Sprover/Sprover) :- P > N, !.
    filename_fofs_single(I,N,F,Sprover0/Sprover) :- I =< N, J is I + 1,
        atom_concat(F,I,FI),
        atom_concat(FI,'.fof',FIfof),

        (   create_fof_files
        ->  write('creating file '),writeln(FIfof),
            create_fof_file(F,FIfof,I)
        ;   write('reading existing file '),writeln(FIfof) ),

        prover_cmd(FIfof,SuffixedCmd),

        shell(SuffixedCmd,StatusProver),
        write('target conjecture: "'), write(FIfof),
        write('" apply prover command: "'), write(SuffixedCmd),
        write('" status code: "'), write(StatusProver),
        write('", successful application (yes: 1, no: 0): '),
        (   StatusProver == 0
        ->  write(1)
        ;   write(0) ),
        nl,

        (StatusProver == 0
        -> Sprover1 is Sprover0 + 1
        ; Sprover1 is Sprover0),
        filename_fofs_single(J,N,F,Sprover1/Sprover).

/*
filename_fofs_single('member/member').
filename_fofs_single('add_peano/add_peano').
%
filename_fofs_single('even_odd/even_odd').
*/

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % override_fof_files():
        %   Succeeds when fof files must be created
        %   i.e. when 'WRITE_FOF' value has been declared as environment variable
        %   or fils

        create_fof_files :-
            (   getenv('WRITE_FOF_FILE', _)
            ->  true
            ;   fail ).

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % output_file_suffix(-Suffix):
        %   Unify Suffix
        %   with environment variable 'SUFFIX' value
        %   or halts execution after printing an error message

        output_file_suffix(Suffix) :-
            (   getenv('SUFFIX', Suffix)
            ->  true
            ;   writeln('Error, environment variable "SUFFIX" is required'),
                halt
            ).

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % output_file(-OutputFile):
        %   Adds suffix to output file
        %   declared as environment variable 'SUFFIX'
        %   or halts execution after printing an error message

        output_file(OutputFile) :-
            output_file_suffix(Suffix),
            atom_concat('tmp_output', Suffix, OutputFile).

        /*
        output_file(-'tmp_output_vampire').
        output_file(-'tmp_output_e_prover').
        */

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % prover_cmd(+FIfof,-RunProverCmd):
        %   Prepares command to apply a prover
        %   to .fof file (first-order form) from
        %   - +FIfof, target file name
        %   - prover execution output file
        %   - and prover command
        %     declared as "PROVER" environment variable
        %     or halts execution after printing an error message

        prover_cmd(FIfof,RunProverCmd) :-
            (   getenv('PROVER', Cmd)
            ->  true
            ;   writeln('Error, environment variable "PROVER" is required'),
                halt
            ),
            output_file(OutputFile),
            atom_concat(Cmd, FIfof, CmdAndFIfof),
            atom_concat(' >> $(pwd)/out/', OutputFile, RedirectOutput),
            atom_concat(CmdAndFIfof, RedirectOutput, RunProverCmd).

        /*
        prover_cmd(+'./src/suffix/suffix-all1.fof', -'eprover --auto --cpu-limit=10 -s ./src/suffix/suffix-all1.fof >> $(pwd)/out/tmp_output_e_prover').
        prover_cmd(+'./src/suffix/suffix-all1.fof', -'vampire --proof off --time_limit 10s ./src/suffix/suffix-all1.fof >> $(pwd)/out/tmp_output_vampire').
        */

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % truncate_output_file():
        %   Removes output file when it exists
        %   before recreating a new one in ./out/ directory.
        %   The output filename starts with 'tmp_output_'
        %   and ends with a suffix declared as environment variable 'SUFFIX'.
        %   See output_file/1.

        truncate_output_file :-
            output_file(OutputFile),
            concat('rm -f $(pwd)/out/', OutputFile, RemoveOutputFile),
            concat('touch $(pwd)/out/', OutputFile, CreateOutputFile),
            shell(RemoveOutputFile),
            shell(CreateOutputFile).
