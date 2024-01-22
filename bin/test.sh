#!/usr/bin/env bash
set -Eeu

. './bin/check.sh'
. './bin/clean.sh'
. './bin/build_pl.sh'
. './bin/build_pr.sh'
. './bin/build.sh'
. './bin/parse_results.sh'
. './bin/print.sh'
. './bin/rename.sh'

export VERBOSE=

function assert {
  expected="${1}"
  actual="${2}"
  message="${3}"

  VERBOSE=1
  if [ "${expected}" = "${actual}" ];
  then
    print_in_green 'OK %s' "${message}"
    remove_lock_files >> /dev/null 2>&1
  else
    print_in_red 'FAILURE: %s' "'${message}'"
    print_in_red 'actual: %s' "'${actual}'"
    print_in_red 'expected: %s' "'${expected}'"
    remove_lock_files >> /dev/null 2>&1

    return 1
  fi
  VERBOSE=
}

function run_tests {
  remove_lock_files >> /dev/null 2>&1
	actual=$(count_requirements "./src/list/list.pl");expected=2;message='count_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(count_requirements "./src/sort/mergesort.pl");expected=6;message='count_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/ackermann/ackermann.pl" | \grep -c "nat.thm");expected=0;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/ackermann/ackermann.pl" | \grep -c "nat.gr");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "list.gr");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "list.thm");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "permutation.gr");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "permutation.thm");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "nat.thm");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "nat.gr");expected=1;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -cE ".(gr|thm)$");expected=6;message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual="$(list_requirements "./src/nat/nat.pl")";expected='';message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_requirements "./src/nat/nat-all.pl");expected='src/nat/nat.gr';message='list_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(list_assumptions './src/nat/nat.pl' | \grep -c '_pred');expected=2;message='list_assumptions';assert "${expected}" "${actual}" "${message}"
	actual=$(list_assumptions './src/nat/nat-all.pl' | \grep -c '_pred');expected=2;message='list_assumptions';assert "${expected}" "${actual}" "${message}"
	actual=$(list_assumptions './src/reverse/reverse.pl' | \grep -c '_pred');expected=7;message='list_assumptions';assert "${expected}" "${actual}" "${message}"
	actual=$(list_assumptions './src/sort/mergesort.pl' | \grep -c '_pred');expected=6;message='list_assumptions';assert "${expected}" "${actual}" "${message}"
	actual=$(ls -1 './src/gcd/gcd-all'*.fof 2> /dev/null | grep --count '');expected=11;message='build_fof';assert "${expected}" "${actual}" "${message}"
	actual=$(ls -1 './src/int/int-all'*.fof 2> /dev/null | grep --count '');expected=67;message='build_fof';assert "${expected}" "${actual}" "${message}"
	actual=$(ls -1 './src/nat/nat-all'*.fof 2> /dev/null | grep --count '');expected=91;message='build_fof';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep "('lemma" -c);expected=1;message='build_fof';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep ' =>' | grep ' s(' | grep -v 'succeeds\|terminates' | grep -c ' = ');expected=1;message='build_fof__axiom_1_clark_equality_theory';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep ' = ' | grep '~ \|]: ~' | grep -cv 'succeeds\|terminates\|fails');expected=6;message='build_fof__axiom_2_clark_equality_theory';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'gr(' | grep -cv '<=>');expected=2;message='build_fof__axiom_4_gr_axioms_constant';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'gr(' | grep -c '<=>');expected=2;message='build_fof__axiom_5_gr_m_ary_functions';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep -c ': ~');expected=6;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c '@<');expected=1;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c '@=<');expected=1;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'nat_succeeds');expected=1;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'nat_list');expected=1;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'plus_');expected=1;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'succeeds' | grep 'fails' | grep '&' | grep ': ~' | grep -c 'times_');expected=1;message='build_fof__axiom_6_uniqueness_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep '@<_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep '@=<_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'nat_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'nat_list_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'plus_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep 'terminates' | grep ' => ' | \grep 'times_succeeds' | grep -c fails);expected=1;message='build_fof__axiom_7_totality_axiom';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep '<=>' | grep 'fails' | grep '[0-9]\+,axiom' | grep -cv terminates);expected=6;message='build_fof__axiom_8_fixed_point_axiom_for_user_defined_predicate_fails';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep '<=>' | grep 'succeeds' | grep -c '[0-9]\+,axiom');expected=6;message='build_fof__axiom_8_fixed_point_axiom_for_user_defined_predicate_succeeds';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep '<=>' | grep 'terminates' | grep -c '[0-9]\+,axiom');expected=6;message='build_fof__axiom_8_fixed_point_axiom_for_user_defined_predicate_terminates';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all1.fof | grep '(induction' -c);expected=1;message='build_fof__axiom_9_induction_schema_for_user_defined_predicate';assert "${expected}" "${actual}" "${message}"
	actual=$(\cat ./src/nat/*all91.fof | grep 'corollary\|lemma\|theorem' | grep axiom -c);expected=90;message='build_fof__assumptions_from_previous_conjectures';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_invalid_time_limit 1 2>> /dev/null && echo $?);expected=0;message='guard_against_invalid_time_limit';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_invalid_time_limit 60 2>> /dev/null && echo $?);expected=0;message='guard_against_invalid_time_limit';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_invalid_time_limit 10 2>> /dev/null && echo $?);expected=0;message='guard_against_invalid_time_limit';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_invalid_time_limit 3 2>> /dev/null || echo $?);expected=1;message='guard_against_invalid_time_limit';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_invalid_time_limit -3 2>> /dev/null || echo $?);expected=1;message='guard_against_invalid_time_limit';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_invalid_time_limit "" 2>> /dev/null || echo $?);expected=1;message='guard_against_invalid_time_limit';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_incomplete_fof_file ./src/nat/nat.pl 2>> /dev/null);expected='./src/nat/nat-all.pl';message='guard_against_incomplete_fof_file';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_incomplete_fof_file ./src/nat/na.pl 2>> /dev/null || echo $?);expected=1;message='guard_against_incomplete_fof_file';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_missing_placeholder_in_proof_file './src/nat/nat.pl' 2>> /dev/null && echo $?);expected=0;message='guard_against_missing_placeholder_in_proof_file';assert "${expected}" "${actual}" "${message}"
	actual=$(guard_against_missing_placeholder_in_proof_file './src/nat/nat-all.pl' 2>> /dev/null || echo $?);expected=1;message='guard_against_missing_placeholder_in_proof_file';assert "${expected}" "${actual}" "${message}"
	actual=$(PATH= check_requirements './src/nat/nat.pl' 2>> /dev/null | grep -c 'bash');expected=1;message='check_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(PATH= check_requirements './src/nat/nat.pl' 2>> /dev/null | grep -c 'Vampire');expected=1;message='check_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(PATH= check_requirements './src/nat/nat.pl' 2>> /dev/null | grep -c 'E Theorem Prover');expected=1;message='check_requirements';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/gcd/gcd-all.pl' 1 2> /dev/null);expected=11;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/gcd/gcd-all.pl' 10 2> /dev/null);expected=11;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/gcd/gcd-all.pl' 60 2> /dev/null);expected=11;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/int/int-all.pl' 1 2> /dev/null);expected=67;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/int/int-all.pl' 10 2> /dev/null);expected=67;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/int/int-all.pl' 60 2> /dev/null);expected=67;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/nat/nat-all.pl' 1 2> /dev/null);expected=91;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/nat/nat-all.pl' 10 2> /dev/null);expected=91;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(count_conjectures_to_prove './src/nat/nat-all.pl' 60 2> /dev/null);expected=91;message='count_conjectures_to_prove';assert "${expected}" "${actual}" "${message}"
	actual=$(combine_results './src/ackermann/ackermann-all.pl' 60 2> /dev/null);expected=33.33333333;message='combine_results';assert "${expected}" "${actual}" "${message}"
	actual=$(combine_results './src/gcd/gcd-all.pl' 60 2> /dev/null);expected=45.45454545;message='combine_results';assert "${expected}" "${actual}" "${message}"
	actual="$(_newline)";expected="$(print_utf8 $'\n')";message='_newline';assert "${expected}" "${actual}" "${message}"
	actual="$(_newline_char)";expected='\n';message='_newline_char';assert "${expected}" "${actual}" "${message}"
	actual="$(print_to_stderr 'subject' 2>&1)";expected='subject';message='print_to_stderr';assert "${expected}" "${actual}" "${message}"
	actual="$(VERBOSE=1 print_separator ' ${SUFFIX}' 2>&1)";expected="$(print_utf8 '%s%s%s' $'\n' '------ ${SUFFIX}' $'\n')";message='print_separator';assert "${expected}" "${actual}" "${message}"
	actual="$(VERBOSE=1 print_subject_is_overwritten './src/nat/nat-all.pr' 2>&1)";expected="$(print_utf8 '%s%s%s%s' "$(_blue_sequence)" "'./src/nat/nat-all.pr' has been overwritten." $'\n' "$(_reset_sequence)")";message='print_subject_is_overwritten';assert "${expected}" "${actual}" "${message}"
	actual="$(print_utf8 '%.0f' '45.4')";expected="45";message='print_utf8';assert "${expected}" "${actual}" "${message}"
	actual="$(print_utf8 '%.0f' '45.5')";expected="46";message='print_utf8';assert "${expected}" "${actual}" "${message}"
	actual="$(print_utf8 '%.0f' '45')";expected="45";message='print_utf8';assert "${expected}" "${actual}" "${message}"
	actual="$(PRINT_LOCALE='fr_FR.UTF-8' print_utf8 '%.0f' '45.7' 2>&1 | grep 'invalid number' -c)";expected="1";message='print_utf8';assert "${expected}" "${actual}" "${message}"
	actual="$(output_file_prefix './src/nat/nat-all.pl')";expected='nat';message='output_file_prefix';assert "${expected}" "${actual}" "${message}"
	actual="$(output_file_suffix './src/nat/nat.pl')";expected='./src/nat/nat-all.pl';message='output_file_suffix';assert "${expected}" "${actual}" "${message}"
	actual="$(program_to_proof './src/nat/nat.pl')";expected='./src/nat/nat.pr';message='program_to_proof';assert "${expected}" "${actual}" "${message}"
	actual="$(program_to_destination_proof './src/nat/nat.pl')";expected='./src/nat/nat-all.pr';message='program_to_destination_proof';assert "${expected}" "${actual}" "${message}"
	actual="$(program_to_lock_file './src/nat/nat.pl')";expected='./out/requirements-for-src_DIR_SEP_nat_DIR_SEP_nat.pr.lock';message='program_to_lock_file';assert "${expected}" "${actual}" "${message}"
	actual="$(program_to_lock_file "src/nat/nat.pl")";expected='./out/requirements-for-src_DIR_SEP_nat_DIR_SEP_nat.pr.lock';message='program_to_lock_file';assert "${expected}" "${actual}" "${message}"
	actual="$(from_gr_thm_to_pl './src/nat/nat.thm')";expected='./src/nat/nat.pl';message='from_gr_thm_to_pl';assert "${expected}" "${actual}" "${message}"
	actual="$(from_gr_thm_to_pl './src/nat/nat.gr')";expected='./src/nat/nat.pl';message='from_gr_thm_to_pl';assert "${expected}" "${actual}" "${message}"
	actual="$(without_extension './src/nat/nat.pl')";expected='./src/nat/nat';message='without_extension';assert "${expected}" "${actual}" "${message}"
	actual="$(without_pl_suffix './src/nat/nat.pl')";expected='nat';message='without_pl_suffix';assert "${expected}" "${actual}" "${message}"
  remove_lock_files >> /dev/null 2>&1
}
run_tests

set +Eeu
