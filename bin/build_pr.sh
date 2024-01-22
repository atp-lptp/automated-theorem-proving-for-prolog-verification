#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/build_pl.sh'

# List recursively axioms and conjectures (axioms, lemmas, corollaries, definitions and theorems)
# required by LPTP proof file (.pr extension)
# associated with logic program (with .pl extension) passed as argument
# by querying 'list_assumptions.' goal with SWI-Prolog
# with input ./bin/build_pr.pl program
# and recursively calling itself for each requirement.
#
# Each assumption term is converted into an axiom directive being written to standard output:
#
# ```prolog
# :- axiom(Reference, Formula).
# ```
#
# Example:
#
# ```shell
# list_assumptions './src/nat/nat.pl'
# ```
#
# For instance, write to standard output all assumptions
# required by ./src/int/int.pr as LPTP axioms
#
# ```prolog
# :- axiom(leq:antisymmetric, all[x,y]:succeeds?x@=< ?y&succeeds?y@=< ?x=> ?x= ?y).
# % [...]
# ```
#
# Example:
#
# ```shell
# actual=$(list_assumptions './src/nat/nat.pl' | \grep -c '_pred');expected=2;message='list_assumptions'
# actual=$(list_assumptions './src/nat/nat-all.pl' | \grep -c '_pred');expected=2;message='list_assumptions'
# actual=$(list_assumptions './src/reverse/reverse.pl' | \grep -c '_pred');expected=7;message='list_assumptions'
# actual=$(list_assumptions './src/sort/mergesort.pl' | \grep -c '_pred');expected=6;message='list_assumptions'
# ```
function list_assumptions {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local exclusion_pattern
  exclusion_pattern="${2}"

  if [ -z "${exclusion_pattern}" ];
  then
    exclusion_pattern='^$'
  fi

  print_utf8 '%s' "$(
    local proof_file
    proof_file=$(program_to_proof "${logic_program}" | sed -E 's#.pr$##g')

    export SOURCE_FILE="${proof_file}"
    $(which swipl) -s ./bin/build_pr.pl -g 'list_assumptions.'

    for req in $(list_requirements "${logic_program}" "${exclusion_pattern}" | grep -vE '\.gr$' | from_gr_thm_to_pl | sort | uniq);
    do
      list_assumptions "${req}" "${exclusion_pattern}" | sed -E 's#:th,#,#g' | sed -E 's#:lm,#,#g' | sed -E 's#:co,#,#g'
      exclusion_pattern="${exclusion_pattern}|/$(without_pl_suffix "${req}")\)"
      print_in_blue '[list_assumptions] ${exclusion_pattern}: %s' "${exclusion_pattern}"
      print_in_blue '[list_assumptions] ${req}: %s' "${req}"
    done
  )" | sort | uniq
}

# Write to standard output all characters from a LPTP proof file
# before the occurrence of the '% __placeholder__' fixed string.
#
# The LPTP proof file (with .pr extension) matches
# a logic program (with .pl extension),
# which filepath is passed as argument
#
# In absence of such a fixed string, the whole LPTP proof file is written to standard output.
# A single placeholder is assumed to be present in the LPTP proof file.
function before_placeholder {
  \cat "$(program_to_proof "${1}")" | \
  tr $'\n' '\0' | \
  sed -E 's#(.*)% __placeholder__.*#\1#g' | \
  tr '\0' $'\n'
}

# Write to standard output part all characters from a LPTP proof file
# after the occurrence of the '% __placeholder__' fixed string.
#
# The LPTP proof file (with .pr extension) matches
# a logic program (with .pl extension),
# which filepath is passed as argument
#
# In absence of such a fixed string, the whole LPTP proof file is written to standard output.
# A single placeholder is assumed to be present in the LPTP proof file.
function after_placeholder {
  \cat "$(program_to_proof "${1}")" | \
  tr $'\n' '\0' | \
  sed -E 's#.*% __placeholder__(.*)#\1#g' | \
  tr '\0' $'\n'
}

# Replace '% __placeholder__' fixed string
# with assumptions required by LPTP proof file (with .pr extension)
# associated with logic program (with .pl extension) passed as argument,
# before writing the result to a target file ending with '-all.pr'
# and prefixed with the same basename as the logic program.
#
# Required assumptions are listed by calling `list_assumptions` function.
#
# Example:
#
# ```shell
# VERBOSE=1 NO_INTERACTION=1 build_pr './src/nat/nat.pl'
# ```
# ```stderr
# Requirements for ./src/nat/nat.pl
# No requirements found.
# ------
# ./src/nat/nat-all.pr has been overwritten.
# ```
#
# In './src/nat/nat.pr',
# '% __placeholder__' is replaced
# with all assumptions required by this LPTP proof file
# before writing the result to `./src/nat/nat-all.pr`.
# None is required for './src/nat/nat.pr'.
#
# When `NO_INTERACTION` is unset, a prompt asks for confirmation
# before overriding the target file.
function build_pr {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local target_file
  target_file="$(program_to_destination_proof "${logic_program}")"

  if ! guard_against_missing_placeholder_in_proof_file "${logic_program}";
  then
    return 1;
  fi

  remove_lock_files

  # For loop Separator set to newline
  IFS=$'\n'

  local choice
  choice="$(ask_for_confirmation 'Do you confirm target file override? ("'"${target_file}"'")')"

  local exclusion_pattern
  exclusion_pattern='^$'

  case "${choice}" in
    y|Y )

      {
        before_placeholder "${logic_program}"
        (
          for req in $(list_requirements "${logic_program}" "${exclusion_pattern}" | grep -vE '\.gr$' | from_gr_thm_to_pl | sort | uniq);
          do
            list_assumptions "${req}" "${exclusion_pattern}" | sed -E 's#:th,#,#g' | sed -E 's#:lm,#,#g' | sed -E 's#:co,#,#g'
            exclusion_pattern="${exclusion_pattern}|/$(without_pl_suffix "${req}")\)"
            print_in_blue '[build_pr] ${exclusion_pattern}: %s' "${exclusion_pattern}"
            print_in_blue '[build_pr] ${req}: %s' "${req}"
          done
        ) | sort | uniq
        after_placeholder "${logic_program}"
      } > "${target_file}"
      print_in_blue "'${target_file}' has been overwritten."

    ;;
    n|N ) echo "no";;
    * ) echo "invalid";;
  esac

  remove_lock_files
}

set +Eeu
