#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/build.sh'

# Find pid of commands containing prover.pl, eprover or vampire
# without matching the grep command itself
# and send SIGTERM, or SIGKILL signal to each of them
# after checking if the process is still running
function stop {
  ps ax | \
  grep 'pr[o]ve.pl\|epr[o]ver\|vamp[i]re' | \
  awk '{print $1}' | \
  xargs -I{} bash -c 'ps -p ${1} > /dev/null && ( kill -15 ${1}  || kill -9 {1} )' shell {}
}

# Apply prover to try demonstrating as many conjectures as possible
# located in a LPTP proof file (with .pr extension)
# with a given time limit passed as 2nd argument.
#
# @param Command template used to execute prover command
#        (this template contains "_TIMEOUT_" placeholder,
#        to be replaced with actual timeout value in seconds)
# @param Time limit in seconds,
#        The "_TIMEOUT_" placeholder in the command template
#        is replaced with this value.
# @param Suffix to be appended to a regular file without extension.
#        The executed prover command results is appended to this file
#        (located in the ./out directory and with filename starting with "tmp_output_")
#        See src/prove.pl:output_file/1 responsible
#        for using this suffix exported as an environment variable.
# @param Logic program filepath (with .pl extension)
#        having associated LPTP proof file (with .pr extension)
#        and related conjectures in .fof files
function apply_prover {
  local cmd
  cmd="${1}"

  if [ -z "${cmd}" ];
  then
    apply_prover__print_help
    print_utf8 'A %s is expected as %s (%s) e.g.%s' 'non-empty string' '1st argument' 'Command template to run prover' $'\n' 1>&2
    print_utf8 '%s' $'\n' 1>&2
    print_utf8 '%s %s' 'vampire --proof off --time_limit _TIMEOUT_s ' $'\n' 1>&2
    print_utf8 '%s %s' 'eprover --auto --cpu-limit=_TIMEOUT_ -s ' $'\n' 1>&2

    return 1
  fi

  local time_limit
  time_limit="${2}"

  if [ -z "${time_limit}" ];
  then
    apply_prover__print_help
    print_utf8 'An %s is expected as %s (%s).%s' 'integer' '2nd argument' 'time_limit in seconds' $'\n' 1>&2

    return 1
  fi

  local suffix
  suffix="${3}"

  if [ -z "${suffix}" ];
  then
    apply_prover__print_help
    print_utf8 'A %s is expected as %s (%s).%s' 'non-empty string' '3rd argument' 'output file suffix' $'\n' 1>&2

    return 1
  fi

  local logic_program
  logic_program="${4}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  (
    # Kill process matching prover.pl interpretation on SIGINT (Ctrl+C), SIGTERM, ERR
    trap 'trap - SIGINT SIGTERM ERR; stop; exit 1' SIGINT SIGTERM ERR;

    export PROVER_TPL="${cmd}"
    export TIMEOUT="${time_limit}"
    export SUFFIX="${suffix}"

    local PROVER_COMMAND
    PROVER_COMMAND="$(echo "${PROVER_TPL}" | sed -E 's#_TIMEOUT_#'"${TIMEOUT}"'#g')"

    export PROVER="${PROVER_COMMAND}"

    local logic_program_without_extension
    logic_program_without_extension="$(echo "${logic_program}" | sed -E  's#\.pl$##g')"

    local goal_argument
    goal_argument="$(print_utf8 "filename_fofs_run_single_prover('%s')." "${logic_program_without_extension}")"

    local prove_cmd
    prove_cmd="$(print_utf8 '%s swipl -s ./src/prove.pl -g "%s"%s' "$(which env)" "${goal_argument}" $'\n')"

    bash -c "${prove_cmd}" 2>&1 | grep 'command:\|ratio' | \
    sed -E 's#'"$(which vampire)"'#vampire#' | \
    sed -E 's#'"$(which eprover)"'#eprover#'
  )
}

# Apply prover to show as many lemmas, corollaries and theorems
# located in a LPTP proof file (with .pr extension) as possible
# with predefined time limits (1s, 10s, 60s)
#
# @param Command template used to execute prover command
#        (this template contains "_TIMEOUT_" placeholder,
#        to be replaced with actual timeout value in seconds).
# @param Suffix to be appended to a regular file with .out extension.
#        The executed prover command results is appended to this file
#        (located in the ./out directory).
# @param Logic program filepath (with .pl extension)
#        having associated LPTP proof file (with .pr extension)
#        and related conjectures in .fof files
function apply_prover_with_predefined_time_limits {
  local cmd
  cmd="${1}"

  local suffix
  suffix="${2}"

  local logic_program
  logic_program="${3}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  declare -a time_limits
  time_limits=(1 10 60)

  unset TIMEOUT

  # See ./bin/check.sh
  logic_program="$(guard_against_incomplete_fof_file "${logic_program}")"

  local output_result

  rm -f "${output_result}"

  local prover_name
  prover_name="$(print_utf8 '%s' "${suffix}" | tr -d '_')"

  local timeout
  for timeout in "${time_limits[@]}"; do
    print_utf8 'Starting "%s" prover (logic program="%s", time limit=%ss).%s' "${prover_name}" "${logic_program}" ${timeout} $'\n'

    output_result="./out/$(without_pl_suffix "${logic_program}")_${timeout}${suffix}.out"
    apply_prover "${cmd}" "${timeout}" "${suffix}" "${logic_program}" | tee "${output_result}.tmp"

    local success_rate
    success_rate="$(\grep 'ratio:' "${output_result}.tmp")."

    print_utf8 'Stopping "%s" prover (logic program="%s", time limit=%ss): %s%s' \
      "${prover_name}" \
      "${logic_program}" \
      "${timeout}" \
      "${success_rate}" \
      $'\n'

    mv "${output_result}"{.tmp,}
  done
}

# Apply provers for all programs available
function apply_prover_for_each_program_with_predefined_time_limits {
  local cmd
  cmd='echo Building FOF file for "${1}"; export NO_INTERACTION=1 VERBOSE= LOGIC_PROGRAM="${1}"; make apply-provers 2> /dev/null || exit 255'
  find ./src/*/*pl \( -path '*-all.pl' \) \
      -exec bash -c "${cmd}" \
      shell {} \;
}

set +Eeu
