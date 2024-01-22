#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/build_pr.sh'
. './bin/check.sh'

# Generate files containing *first-order form* conjectures, and axioms
# from logic program passed as 1st argument (with .pl extension).
#
# When the 1st argument is not suffixed with '-all.pl',
# it is renamed on the fly by appending '-all.pl'
# to the original basename of the logic program.
#
# The argument or the renaming results
# is then passed to ./src/indp.sh script
#
# Logic program are suffixed with '-all.pl'
# by running the following command aiming
# at inserting missing requirements (assumptions):
# ```shell
# LOGIC_PROGRAM='./src/nat/nat.pl' `make build`
# ```
#
# Examples:
#
# ```
# actual=$(ls -1 './src/gcd/gcd-all'*.fof 2> /dev/null | grep --count '');expected=11;message='build_fof'
# actual=$(ls -1 './src/int/int-all'*.fof 2> /dev/null | grep --count '');expected=67;message='build_fof'
# actual=$(ls -1 './src/nat/nat-all'*.fof 2> /dev/null | grep --count '');expected=91;message='build_fof'
# actual=$(\cat ./src/nat/*all1.fof | grep "('lemma" -c);expected=1;message='build_fof'
# actual=$(\cat ./src/nat/*all1.fof | grep ' =>' | grep ' s(' | grep -v 'succeeds\|terminates' | grep -c ' = ');expected=1;message='build_fof__axiom_1_clark_equality_theory'
# actual=$(\cat ./src/nat/*all1.fof | grep ' = ' | grep '~ \|]: ~' | grep -cv 'succeeds\|terminates\|fails');expected=6;message='build_fof__axiom_2_clark_equality_theory'
# actual=$(\cat ./src/nat/*all1.fof | grep 'gr(' | grep -cv '<=>');expected=2;message='build_fof__axiom_4_gr_axioms_constant'
# actual=$(\cat ./src/nat/*all1.fof | grep 'gr(' | grep -c '<=>');expected=2;message='build_fof__axiom_5_gr_m_ary_functions'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep -c ': ~');expected=6;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c '@<');expected=1;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c '@=<');expected=1;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'nat_succeeds');expected=1;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'nat_list');expected=1;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'plus_');expected=1;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'times_');expected=1;message='build_fof__axiom_6_uniqueness_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep '@<_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep '@=<_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'nat_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'nat_list_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'plus_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'times_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom'
# actual=$(\cat ./src/nat/*all1.fof | grep '<=>' | grep 'fails' | grep '[0-9]\+,axiom' | grep -cv terminates);expected=6;message='build_fof__axiom_8_fixed_point_axiom_for_user_defined_predicate_fails'
# actual=$(\cat ./src/nat/*all1.fof | grep '<=>' | grep 'succeeds' | grep -c '[0-9]\+,axiom');expected=6;message='build_fof__axiom_8_fixed_point_axiom_for_user_defined_predicate_succeeds'
# actual=$(\cat ./src/nat/*all1.fof | grep '<=>' | grep 'terminates' | grep -c '[0-9]\+,axiom');expected=6;message='build_fof__axiom_8_fixed_point_axiom_for_user_defined_predicate_terminates'
# actual=$(\cat ./src/nat/*all1.fof | grep '(induction' -c);expected=1;message='build_fof__axiom_9_induction_schema_for_user_defined_predicate'
# actual=$(\cat ./src/nat/*all91.fof | grep 'corollary\|lemma\|theorem' | grep axiom -c);expected=90;message='build_fof__assumptions_from_previous_conjectures'
# ```
function build_fof {
    local logic_program
    logic_program="${1}"

    if ! guard_against_invalid_logic_program "${logic_program}"; then
        return 1
    fi

    # See ./bin/check.sh
    logic_program="$(guard_against_incomplete_fof_file "${logic_program}")"
    if [ $? -ne 0 ]; then
        return 1
    fi

    local without_extension
    without_extension="$(without_extension "${logic_program}")"

    ./src/indp.sh "${without_extension}"
}

# Given a logic program (with .pl extension)
# which relative filepath is passed as `build_requirements` 1st argument
#
# 1. concatenate the logic program associated LPTP proof file requirements (having .pl extensions)
# and write the concatenation result
# in a target file having filename
# - prefixed with the original logic program basename
# - suffixed with '-all.pl',
#
# 2. concatenate conjectures (lemmas, corollaries, theorems) and axioms (from files having .pr extension)
# required by the logic program associated LPTP proof file
# and write the concatenation result
# to a target file having filename
# - prefixed with the original logic program basename
# - suffixed with '-all.pr'.
#
# See "build_pr" function in ./bin/build_pr.sh.
function build_requirements {
    local logic_program
    logic_program="${1}"

    if ! guard_against_invalid_logic_program "${logic_program}"; then
        return 1
    fi

    # Run make stop on SIGINT, SIGTERM, ERR signal trap
    trap 'trap - SIGINT SIGTERM ERR; echo "Build failed."; exit 1' SIGINT SIGTERM ERR

    build_pl "${logic_program}"
    build_pr "${logic_program}"
}

# Given a logic program (with .pl extension) passed as 1st argument,
# Call `build_requirements` and `build_fof` with the same argument.
function build {
    local logic_program
    logic_program="${1}"

    if ! guard_against_invalid_logic_program "${logic_program}"; then
        return 1
    fi

    build_requirements "${logic_program}"
    build_fof "$(output_file_suffix "${logic_program}")"
}

# Build './bin/test.sh' from function comments
# before running test script
function run_tests {
    local body
    body="$(
        \cat ./bin/{build_pl,build_pr,build,check,parse_results,print,rename}.sh |
            grep -E '^# (test |actual=)' |
            sed -E 's/\# test /test /g' |
            sed -E 's#(.+)$#\t\1;assert "${expected}" "${actual}" "${message}"#g' |
            sed -E 's/# actual/actual/g'
    )"

    (
        \cat ./bin/test_tpl.sh |
            tr $'\n' '\0' |
            sed -E 's#(.*)_body_.*#\1#g' |
            tr '\0' $'\n'

        print_utf8 '%s' "${body}"

        \cat ./bin/test_tpl.sh |
            tr $'\n' '\0' |
            sed -E 's/.*_body_(.*)/\1/g' |
            tr '\0' $'\n'
    ) >./bin/test.sh

    if ! bash -n './bin/test.sh'; then
        print_in_red 'Failed on test extraction.'
        return 1
    fi

    bash -c '. ./bin/test.sh'

    local failures
    failures=$(bash -c '. ./bin/test.sh' 2>&1 | grep 'FAILURE:' -c)

    if [ ${failures} -eq 0 ]; then
        echo 'All tests passed.'
    else
        echo 'Some tests failed.'
        return 1
    fi
}

# Build all -all.pl, -all.pr, .fof files
# from logic programs and their associated LPTP proof files
function build_all {
    find ./src/*/*pl \( -not -path '*-all.pl' \) \
        -exec bash -c 'echo Building requirements for "${1}"; export NO_INTERACTION=1 VERBOSE= LOGIC_PROGRAM="${1}"; make build >> /dev/null 2>&1 || exit 255' \
        shell {} \;
}

# Build all .fof files
# from logic programs and their associated LPTP proof files
function build_all_fof {
    find ./src/*/*pl \( -not -path '*-all.pl' \) \
        -exec bash -c 'echo Building FOF file for "${1}"; export NO_INTERACTION=1 VERBOSE= LOGIC_PROGRAM="${1}"; make build-fof 2> /dev/null || exit 255' \
        shell {} \;
}

set +Eeu
