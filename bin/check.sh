#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/print.sh'
. './bin/rename.sh'

# Test if 1st argument is
# - a non-empty string.
# - a regular filepath.
# If not, prints error message and returns 1.
function _guard_against_invalid_logic_program {
    local logic_program
    logic_program="${1}"

    if [ -z "${logic_program}" ];
    then
      print_utf8 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'logic program name' $'\n' 1>&2
      return 1
    fi

    if [ ! -e "${logic_program}" ];
    then
      print_utf8 'A %s is expected as %s (%s).%s' 'regular filepath' '1st argument' 'logic program' $'\n' 1>&2
      return 1
    fi
}

# Test if 1st argument is
# valid according to '_guard_against_invalid_logic_program' function
# If not, invite user to declare LOGIC_PROGRAM environment variable.
function guard_against_invalid_logic_program {
    local logic_program
    logic_program="${1}"

    if ! _guard_against_invalid_logic_program "${logic_program}" >> /dev/null 2>&1 ;
    then
      print_utf8 'A %s is expected (%s).%s' 'regular filepath' 'logic program name' $'\n' 1>&2
      print_utf8 '%s%s' 'Please declare a logic program as environment variable e.g.' $'\n' 1>&2
      print_utf8 '%s%s' 'export LOGIC_PROGRAM=./src/nat/nat.pl' $'\n' 1>&2
      print_utf8 'Actual: %s%s' "${logic_program}" $'\n' 1>&2

      return 1
    fi
}

# Test if 1st argument is
# one of 'lemma', 'corollary' or 'theorem',
# otherwise prints error message and returns 1.
function guard_against_invalid_conjecture_kind {
  local conjecture_kind
  conjecture_kind="${1}"

  if [ "${conjecture_kind}" != 'theorem' ] && [ "${conjecture_kind}" != 'lemma' ] && [ "${conjecture_kind}" != 'corollary' ];
  then
      print_utf8 '%s%s' 'Only "lemma", "corollary" or  "theorem" kinds are valid conjecture kinds.' $'\n' 1>&2

      return 1
  fi
}

# Test if 1st argument is one
# of the time limits predefined for the experiments
# (1, 10 or 60)
# otherwise prints error message and returns 1.
#
# Example:
#
# ```shell
# actual=$(guard_against_invalid_time_limit 1 2>> /dev/null && echo $?);expected=0;message='guard_against_invalid_time_limit'
# actual=$(guard_against_invalid_time_limit 60 2>> /dev/null && echo $?);expected=0;message='guard_against_invalid_time_limit'
# actual=$(guard_against_invalid_time_limit 10 2>> /dev/null && echo $?);expected=0;message='guard_against_invalid_time_limit'
# actual=$(guard_against_invalid_time_limit 3 2>> /dev/null || echo $?);expected=1;message='guard_against_invalid_time_limit'
# actual=$(guard_against_invalid_time_limit -3 2>> /dev/null || echo $?);expected=1;message='guard_against_invalid_time_limit'
# actual=$(guard_against_invalid_time_limit "" 2>> /dev/null || echo $?);expected=1;message='guard_against_invalid_time_limit'
# ```
function guard_against_invalid_time_limit {
  local time_limit
  time_limit="${1}"

  if [ -z "${time_limit}" ];
  then
    print_utf8 'An %s is required as %s (%s).%s' 'integer' '1st argument' 'time limit in seconds' $'\n' 1>&2
    return 1
  fi

  if [ "${time_limit}" != '1' ] && [ "${time_limit}" != '10' ] && [ "${time_limit}" != '60' ];
  then
    print_utf8 'An %s is required as %s (%s).%s' 'integer' '1st argument' '1, 10 or 60' $'\n' 1>&2
    return 1
  fi
}

# Test if utilities commands are available
# otherwise prints error message and returns 1.
function guard_against_unavailable_command {
  if ! command -v bc > /dev/null 2>&1
  then
    print_optionally '[GNU bc](https://www.gnu.org/software/bc/) is required to aggregate results.%s' $'\n' 1>&2
    return 1
  fi
}

# Do nothing if the logic program passed as 1st argument
# is suffixed with '-all.pl',
# otherwise write the associated logic program
# suffixed with '-all.pl' to standard output.
#
# If no logic program suffixed with '-all.pl' exists,
# print an error message and returns 1
#
# Example:
#
# ```shell
# actual=$(guard_against_incomplete_fof_file ./src/nat/nat.pl 2>> /dev/null);expected='./src/nat/nat-all.pl';message='guard_against_incomplete_fof_file'
# actual=$(guard_against_incomplete_fof_file ./src/nat/na.pl 2>> /dev/null || echo $?);expected=1;message='guard_against_incomplete_fof_file'
# ```
function guard_against_incomplete_fof_file {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local target_extension_occurrences
  target_extension_occurrences=$(echo "${logic_program}" | grep --count '\-all.pl')

  if [ ${target_extension_occurrences} -eq 0 ];
  then
    local logic_program_arg
    logic_program_arg="${logic_program}"

    local target_program
    target_program=$(output_file_suffix "${logic_program}")
    #print_in_red 'Applying prover to "%s" instead of "%s".%s' \
    #  "${target_program}" \
    #  "${logic_program_arg}" \
    #  $'\n'
    logic_program="${target_program}"

    # Check again if renaming provides with a valid logic program
    if [ ! -e "${logic_program}" ];
    then
      local without_suffix
      without_suffix="$(without_pl_suffix ${logic_program})"
      print_utf8 '%s: "%s".%s' 'Missing program (after renaming)' "${logic_program}" $'\n' 1>&2
      print_utf8 'To generate "%s.pl", "%s.pr", please run %s```shell%s%s%s```%s' \
        "${without_suffix}" \
        "${without_suffix}" \
        $'\n' \
        $'\n' "LOGIC_PROGRAM='${logic_program_arg}' make build" $'\n' \
        $'\n' \
      1>&2

      return 1
    fi
  fi

  echo "${logic_program}"
}

# Test if the number of output files passed as 1st argument
# is equal to 6, otherwise prints error message and returns 1.
# There is one file
# for each prover (vampire, e)
# and for each time limit in seconds (1, 10, 60)
function guard_against_inconsistent_output_files_count {
  local output_files_count
  output_files_count=${1}

  if [ ${output_files_count} -ne 6 ];
  then
    print_in_red 'Expected 6 matching result files, but found %s.' "${output_files_count}"
    return 1;
  fi
}

# For comparing multiple versions of the same program,
# Exporting SUB_DIR environment variable
# allows to specify the subdirectory of ./out directory
# where output files are located,
# before parsing results, and aggregating them.
# independently from the target logic program
# checked otherwise.
# If SUB_DIR is not a valid subdirectory,
# prints error message and returns 1.
#
# See `join_parsed_results_for_predefined_time_limits` function
function guard_against_invalid_subdirectory {
  local sub_directory=''
  sub_directory="${1}"

  if [ -n "${SUB_DIR}" ]
  then
    if [ ! -d "./out/${SUB_DIR}" ];
    then
      print_utf8 '%s is not a valid subdirectory%s' "./out/${SUB_DIR}" $'\n' 1>&2

      return 1
    else
      sub_directory="$(echo ${SUB_DIR} | sed 's#\/##g')/"
    fi
  fi

  echo "${sub_directory}"
}

# Test if the LPTP proof file associated with the logic program
# passed as 1st argument contains a placeholder:
# ```
# % __placeholder__
# ```
#
# Example:
# ```shell
# actual=$(guard_against_missing_placeholder_in_proof_file './src/nat/nat.pl' 2>> /dev/null && echo $?);expected=0;message='guard_against_missing_placeholder_in_proof_file'
# actual=$(guard_against_missing_placeholder_in_proof_file './src/nat/nat-all.pl' 2>> /dev/null || echo $?);expected=1;message='guard_against_missing_placeholder_in_proof_file'
# ```
function guard_against_missing_placeholder_in_proof_file {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local source_proof_file
  source_proof_file="$(program_to_destination_proof "${logic_program}" | sed -E 's#-all.pr#.pr#g')"

  local placeholder_occurrences
  placeholder_occurrences=$(\grep '% __placeholder__' -c "${source_proof_file}")

  if [ ${placeholder_occurrences} -ne 1 ];
  then
    print_in_red \
    'Placeholder '"'"'%%%%%s'"'"' must be added to '"'"'%s'"'"' so that missing axioms could be inserted automatically.' \
    ' __placeholder__' \
    "$(echo "${source_proof_file}" | sed -E 's#-all.pr#.pr#g')"

    return 1
  fi
}

# Test if
# - vampire is installed and available in $PATH
# - eprover is installed and available in $PATH
# - there are fof files for the logic program
# passed as 1st argument
# otherwise prints error message and returns 1.
#
# Example:
# ```shell
# actual=$(PATH= check_requirements './src/nat/nat.pl' 2>> /dev/null | grep -c 'bash');expected=1;message='check_requirements'
# actual=$(PATH= check_requirements './src/nat/nat.pl' 2>> /dev/null | grep -c 'Vampire');expected=1;message='check_requirements'
# actual=$(PATH= check_requirements './src/nat/nat.pl' 2>> /dev/null | grep -c 'E Theorem Prover');expected=1;message='check_requirements'
# ```
function check_requirements {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local total_fof_files
  total_fof_files=$(ls -1 ./src/*/"$(without_pl_suffix ${logic_program})"*.fof | wc -l)

  if ! command -v vampire >> /dev/null 2>&1 || \
  ! command -v eprover >> /dev/null 2>&1 || \
  ! command -v bash >> /dev/null 2>&1;
  then

    if ! command -v vampire >> /dev/null 2>&1
    then
      print_utf8 '%s.%s' 'Please install [Vampire](https://vprover.github.io/download.html)' $'\n'
    fi

    if ! command -v eprover >> /dev/null 2>&1;
    then
      print_utf8 '%s.%s' 'Please install [E Theorem Prover](https://wwwlehre.dhbw-stuttgart.de/~sschulz/E/Download.html)' $'\n'
    fi

    if ! command -v bash >> /dev/null 2>&1;
    then
      print_utf8 '%s.%s' 'Please install [bash](https://www.gnu.org/software/bash/)' $'\n'
    fi

    return 1
  fi

  if [ "${total_fof_files}" -eq 0 ];
  then
    print_in_red 'No fof file found for "%s".' "${logic_program}"
    print_utf8 '%s%s' 'Did you mean to execute the following command?' $'\n'
    print_utf8 '%s%s' 'LOGIC_PROGRAM='"${logic_program}"' make build' $'\n'

    return 1
  fi
}

set +Eeu
