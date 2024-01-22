#!/usr/bin/env bash
set -Eeu

# Remove .pl extension and -all suffix from 1st argument
# ```shell
# actual="$(output_file_prefix './src/nat/nat-all.pl')";expected='nat';message='output_file_prefix'
# ```
function output_file_prefix {
  without_pl_suffix "${1}" | sed -E 's#-all##g'
}

# Replaced .pl extension with -all.pl
# in 1st argument assumed to be a string
# before writing replacement result to standard output
#
# Example:
#
# ```shell
# actual="$(output_file_suffix './src/nat/nat.pl')";expected='./src/nat/nat-all.pl';message='output_file_suffix'
# ```
function output_file_suffix {
  echo -n "${1}" | sed -E 's#-all\.pl#\.pl#' | sed -E 's#\.pl$#-all.pl#g'
}

# Replaced .pl extension with .pr
# in 1st argument assumed to be a string
# before writing replacement result
# to standard output
#
# Example:
#
# ```shell
# actual="$(program_to_proof './src/nat/nat.pl')";expected='./src/nat/nat.pr';message='program_to_proof'
# ```
function program_to_proof {
  echo -n "${1}" | sed -E 's#\.pl$#.pr#g'
}

# Replaced .pl extension with -all.pr
# in 1st argument assumed to be a string
# before writing replacement result
# to standard output
#
# Example:
#
# ```shell
# actual="$(program_to_destination_proof './src/nat/nat.pl')";expected='./src/nat/nat-all.pr';message='program_to_destination_proof'
# ```
function program_to_destination_proof {
  echo -n "${1}" | sed -E 's#\.pl$#-all.pr#g'
}

# Write to standard output the lock file path associated
# with a logic program
#
# Example:
#
# ```shell
# actual="$(program_to_lock_file './src/nat/nat.pl')";expected='./out/requirements-for-src_DIR_SEP_nat_DIR_SEP_nat.pr.lock';message='program_to_lock_file'
# actual="$(program_to_lock_file "src/nat/nat.pl")";expected='./out/requirements-for-src_DIR_SEP_nat_DIR_SEP_nat.pr.lock';message='program_to_lock_file'
# ```
function program_to_lock_file {
  local proof_file
  proof_file="$(program_to_proof "${1}")"
  print_utf8 '%s' ./out/requirements-for-"$(print_utf8 '%s' "${proof_file}" | \
    sed -E 's#^\.\/##g' | \
    sed -E 's#\/#_DIR_SEP_#g')".lock
}

# Replace .gr extension, or .thm extension with .pl from 1st argument.
# If the 1st argument is empty,
# read from standard input.
# Write result to standard output in both cases.
#
# ```shell
# actual="$(from_gr_thm_to_pl './src/nat/nat.thm')";expected='./src/nat/nat.pl';message='from_gr_thm_to_pl'
# actual="$(from_gr_thm_to_pl './src/nat/nat.gr')";expected='./src/nat/nat.pl';message='from_gr_thm_to_pl'
# ```
function from_gr_thm_to_pl {
  local suffix_substitution_pattern
  suffix_substitution_pattern='s#\.(thm|gr)$#\.pl#'

  if [ -z "${1}" ];
  then
    \cat - | sed -E "${suffix_substitution_pattern}";
    return
  fi

  echo -n "${1}" | sed -E "${suffix_substitution_pattern}"
}

# Write file path passed as argument to standard output
# after removing its suffix (extension not containing any dots)
#
# Example:
#
# ```shell
# actual="$(without_extension './src/nat/nat.pl')";expected='./src/nat/nat';message='without_extension'
# ```
function without_extension {
  echo -n "${1}" | sed -E 's#\.[^\.]+$##g'
}

# Write file basename to standard output.
# Filename assumed to be passed as 1st argument
#
# Example:
#
# ```shell
# actual="$(without_pl_suffix './src/nat/nat.pl')";expected='nat';message='without_pl_suffix'
# ```
function without_pl_suffix {
  basename -s '.pl' "${1}"
}

set +Eeu
