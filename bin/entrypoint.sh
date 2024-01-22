#!/usr/bin/env bash
set -Eeu

# Install Vampire system and z3
function run {
  /usr/local/bin/eprover --version
  /usr/local/bin/vampire --version
  $(which swipl) --version
  /bin/bash --version

  printf '%s.%s' 'Keeping container running' $'\n' 1>&2
  tail -F /dev/null
}
run "$@"

set +Eeu