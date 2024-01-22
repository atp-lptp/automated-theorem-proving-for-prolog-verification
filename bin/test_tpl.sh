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
_body_
  remove_lock_files >> /dev/null 2>&1
}
run_tests

set +Eeu
