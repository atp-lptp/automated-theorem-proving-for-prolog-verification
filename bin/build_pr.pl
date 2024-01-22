
:- op(980,xfy,by).        % ... by ...
:- op(970,xfy,:).         % two Peano dots, right associative
:- op(960,yfx,<=>).       % equivalence
:- op(950,xfy,=>).        % implication (-> is an operator of Prolog)
:- op(940,yfx,\/).        % disjunction (v can be used as name)
:- op(930,yfx,&).         % conjunction
:- op(900,fy,~).          % negation (cf. not, \+)
:- op(900,fy,not).        % negation
:- op(900,fy,def).        % def
:- op(900,fy,succeeds).	  % succeeds
:- op(900,fy,fails).		  % fails
:- op(900,fy,terminates).	% terminates
:- op(800,fy,all).        % universal quantifier
:- op(800,fy,ex).         % existential quantifier
:- op(700,yfx,=).         % equality
:- op(700,xfy,<>).        % different
:- op(700,xfy,<).         % less (built-in)
:- op(700,xfy,=<).        % less than or equal (built-in)
:- op(700,xfy,@<).        % less (nat)
:- op(700,xfy,@=<).       % less than or equal (nat)
:- op(700,xfy,#<).        % less (int)
:- op(700,xfy,#=<).       % less than or equal (int)
:- op(600,yfx,**).        % concatenation
:- op(550,xfy,imp).
:- op(500,yfx,@+).        % sum (nat)
:- op(500,yfx,#+).        % sum (int)
:- op(500,yfx,or).
:- op(500,yfx,#-).        % subtraction (int)
:- op(400,yfx,@*).        % product (nat)
:- op(400,yfx,and).
:- op(400,yfx,#*).        % product (int)
:- op(300,fy,#-).         % minus (int)
:- op(300,fy,neg).
:- op(100,fy,?).          % variables: ?x, ?y, ?z, ?v, ?0, ?1, ?2

:- include('../src/indp.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list_assumptions():
%   List assumption from LPTP proof file targeted
%   by exporting environment variable SOURCE_FILE

list_assumptions :-
    source_file(Source),
    filename_lemmas_defns_axs(Source, Lms, Defns, Axs),
    include(print_lemma, Lms, _),
    include(print_corollary, Lms, _),
    include(print_theorem, Lms, _),
    include(print_axiom, Axs, _),
    include(print_definitions, Defns, _),

    halt.

    % See also ./bin/build_pr.sh:list_assumptions function
    % responsible for removing those suffixes
    % ':lm' matches with lemmas
    % ':co' matches with corollaries
    % ':th' matches with theorems

    print_lemma(Fact) :-
        Fact =.. [_|[lemma-(Ref)|[Form]]],
        maplist(write,[':- axiom(',Ref,':lm, ']),write(Form),writeln(').').

    print_corollary(Fact) :-
        Fact =.. [_|[corollary-(Ref)|[Form]]],
        maplist(write,[':- axiom(',Ref,':co, ']),write(Form),writeln(').').

    print_theorem(Fact) :-
        Fact =.. [_|[theorem-(Ref)|[Form]]],
        maplist(write,[':- axiom(',Ref,':th, ']),write(Form),writeln(').').

    print_axiom(Fact) :-
        Fact =.. [_|[axiom-(Ref)|[Form]]],
        maplist(write,[':- axiom(',Ref,', ']),write(Form),writeln(').').

    print_definitions(Fact) :-
        Fact = def(N,A,Form),
        maplist(write,[':- definition_pred(',N,', ',A, ',']),write(Form),writeln(').').

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % source_file(-Source):
    %   Get source file from environment variable SOURCE_FILE,

    source_file(Source) :-
        getenv('SOURCE_FILE', Source)
        ->  true
        ;   fail.
