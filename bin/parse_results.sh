#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/check.sh'
. './bin/print.sh'
. './bin/rename.sh'

# Parse results in ./out directory,
# matching logic program passed as 1st argument
# and time limit passed as 2nd argument.
#
# Results to parsed are expected to be available
# as `./out/${program_name}-all_${suffix}.out` files
# for each
# - prover associated with "${suffix}"
#   valid "${suffix}" is one of
#   - e_prover
#   - vampire
# - logic program associated with LPTP proof file
#   valid ${program_name} is one of
#   - gcd
#   - list
#   - mergesort
#   - nat
#   - permutation
#   - sort
#   - suffix
#   - taut
function preserve_unique_fof_file_index_and_success_status {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local time_limit
  time_limit="${2}"

  if ! guard_against_invalid_time_limit "${time_limit}";
  then
    return 1
  fi

  local file_prefix
  file_prefix="$(output_file_prefix "${logic_program}")"

  print_optionally '======= BEGIN: %s =========%s' "${time_limit}" $'\n'

  local sub_directory
  sub_directory="$(guard_against_invalid_subdirectory "${SUB_DIR}")"

  local remove_status_code_field
  remove_status_code_field='s# status code: "[0-9]*"##g'

  print_optionally '%s%s' "$(
    \cat ./out/"${sub_directory}${file_prefix}"-all_*.out | \
    grep '='"${time_limit}\s"'\|\s'"${time_limit}"'s' | \
    sed -E 's#successful application \([^()]*\): ##g' | \
    sed -E "${remove_status_code_field}" | \
    sed -E 's#target conjecture: "[^0-9]*([0-9]+\.fof)", #\1,#g'
  )" $'\n'

  print_optionally '======= END: %s =========%s' "${time_limit}" $'\n'

  local how_many_matching_result_files
  how_many_matching_result_files="$(ls -1 ./out/"${sub_directory}${file_prefix}"-all_*.out | grep --count '')"

  if ! guard_against_inconsistent_output_files_count "${how_many_matching_result_files}";
  then
    print_utf8 'Executed command: `%s`%s' 'ls -1 ./out/'"${sub_directory}${file_prefix}"'-all_*.out | grep --count '"''" $'\n' 1>&2
    return 1
  fi

  # Distinct outcomes for the same conjecture
  # in last column will contribute as 1
  # Same outcomes  in last columns will contribute
  # as either 0 or 1
  # after deduplication is applied with 'uniq' filter
  #
  # Examples:
  #
  # By deduplication with uniq filter
  # fof1,0
  # fof1,0
  # is reduced to
  # fof1,0
  # => Not counted
  #
  # 1 more lemma is counted as
  # fof1,1
  # fof1,1
  # is reduced to
  # fof1,1
  #
  # 1 more lemma is counted
  # in both following cases as well
  # fof1,0
  # fof1,1
  # or
  # fof1,1
  # fof1,0
  \cat ./out/"${sub_directory}${file_prefix}"-all_*.out | \
  grep '='"${time_limit}\s"'\|\s'"${time_limit}"'s' | \
  sed -E 's#successful application \([^()]*\): ##g' | \
  sed -E "${remove_status_code_field}" | \
  sed -E 's# apply prover command: "[^"]+"##' | \
  sed -E 's#target conjecture: "[^0-9]*([0-9]+\.fof)", #\1,#g' | \
  sort -n | \
  uniq
}

# Count conjectures to prove
#
# A target logic program (with .pl extension) is passed as 1st argument.
# A time limit in seconds is passed as 2nd argument.
#
# Examples:
#
# ```
# actual=$(count_conjectures_to_prove './src/gcd/gcd-all.pl' 1 2> /dev/null);expected=11;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/gcd/gcd-all.pl' 10 2> /dev/null);expected=11;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/gcd/gcd-all.pl' 60 2> /dev/null);expected=11;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/int/int-all.pl' 1 2> /dev/null);expected=67;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/int/int-all.pl' 10 2> /dev/null);expected=67;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/int/int-all.pl' 60 2> /dev/null);expected=67;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/nat/nat-all.pl' 1 2> /dev/null);expected=91;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/nat/nat-all.pl' 10 2> /dev/null);expected=91;message='count_conjectures_to_prove'
# actual=$(count_conjectures_to_prove './src/nat/nat-all.pl' 60 2> /dev/null);expected=91;message='count_conjectures_to_prove'
# ```
function count_conjectures_to_prove {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local time_limit
  time_limit="${2}"

  if ! guard_against_invalid_time_limit "${time_limit}";
  then
    return 1
  fi

  if ! preserve_unique_fof_file_index_and_success_status "${logic_program}" "${time_limit}" | \
  cut -d ',' -f 1 | \
  sort -n | \
  uniq | \
  grep --count '';
  then
    return 1;
  fi
}

# Parse results in ./out directory.
#
# A target logic program (with .pl extension) is passed as 1st argument.
# This program is expected to be associated
# with a proof file (with .pr exemple),
# processed beforehand to build *first-order form* files.
#
# A time limit in seconds is passed as 2nd argument.
#
# Parsing result is written to standard output as a percentage value.
#
# For more details about generating fof file and applying prover see
# - ./src/indp.pl
# - ./src/prove.pl
# - ./bin/run.sh
#
# Examples:
#
# ```shell
# actual=$(combine_results './src/ackermann/ackermann-all.pl' 60 2> /dev/null);expected=33.33333333;message='combine_results'
# actual=$(combine_results './src/gcd/gcd-all.pl' 60 2> /dev/null);expected=45.45454545;message='combine_results'
# ```
function combine_results {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  local time_limit
  time_limit="${2}"

  if ! guard_against_invalid_time_limit "${time_limit}";
  then
    return 1
  fi

  if ! guard_against_unavailable_command;
  then
    return 1
  fi

  # prevent optional print on call to `count_conjectures_to_prove "${logic_program}" "${time_limit}"`
  local verbose_opt
  verbose_opt="${VERBOSE}"
  unset VERBOSE

  local total_conjectures
  total_conjectures=$(count_conjectures_to_prove "${logic_program}" "${time_limit}")

  if [ ${?} -gt 0 ];
  then
    return 1
  fi

  export VERBOSE="${verbose_opt}"

  local total_proved_conjectures
  total_proved_conjectures=$(echo "scale=8;($(preserve_unique_fof_file_index_and_success_status "${logic_program}" "${time_limit}" | \
  cut -d ',' -f 2 |  \
  tr $'\n' '+')0)" | bc)

  print_optionally \
    'total conjectures proved by either E Theorem Prover or Vampire (each conjecture counted once): %s%s' \
    "${total_proved_conjectures}" \
    $'\n'

  print_optionally \
    'total conjectures to prove: %s%s' \
    "${total_conjectures}" \
    $'\n'

  echo "scale=8;${total_proved_conjectures}*100/${total_conjectures}" | bc
}

# Parse results in ./out directory
# for each predefined time limits
# and for each prover associated with a suffix.
#
# A target logic program (with .pl extension) is passed as 1st argument.
# This program is expected to be associated
# with a LPTP proof file (with .pr extension).
#
# Parsed results are joined before being written to standard output.
#
# Example:
# ```
# join_parsed_results_for_predefined_time_limits './src/gcd/gcd-all.pl'
# SUB_DIR=0.1 join_parsed_results_for_predefined_time_limits './src/0.1_reverse/reverse.pl'
# SUB_DIR=0.1 join_parsed_results_for_predefined_time_limits './src/0.1_sort/sort.pl'
# ```
function join_parsed_results_for_predefined_time_limits {
  local logic_program
  logic_program="${1}"

  if ! guard_against_invalid_logic_program "${logic_program}" ;
  then
    return 1
  fi

  declare -a predefined_time_limits
  predefined_time_limits=(1 10 60)

  local file_prefix
  file_prefix="$(output_file_prefix "${logic_program}")"

  # Declared in ./Makefile
  declare -a suffixes
  suffixes=("${SUFFIX_E_PROVER}" "${SUFFIX_VAMPIRE}" )

  local max_chars
  max_chars=6
  if [ $(print_utf8 '%s' "${file_prefix}" | wc -c) -lt ${max_chars} ];
  then
    print_utf8 '%s'$'\t' "${file_prefix}"
  else
    print_utf8 '%s'$'\t' "$(echo "${file_prefix}" | head -c${max_chars})."
  fi

  local sub_directory
  sub_directory="$(guard_against_invalid_subdirectory "${SUB_DIR}")"

  local how_many_total_lemmas
  how_many_total_lemmas=$(
    \cat ./out/"${sub_directory}${file_prefix}"-all_*"${suffix}".out | \
    grep 'lemmas' | \
    head -n1 | \
    sed -E 's#([0-9]+) lemmas,.+#\1#g'
  )

  print_utf8 '%s'$'\t' ${how_many_total_lemmas}

  local time_limit
  local output_file
  local ratio

  (
    for time_limit in "${predefined_time_limits[@]}"; do
    (
      for suffix in "${suffixes[@]}"; do
        output_file="./out/${sub_directory}${file_prefix}-all_${time_limit}${suffix}.out"
        ratio="$(
          grep 'ratio' "${output_file}" | \
          head -n1 | \
          sed -E 's#.+ratio: (.+)$#\1#g'
        )"
        print_utf8 '%.0f%%%s' "${ratio}" $'\t'
      done

      # Combined results for both provers
      print_utf8 '%.0f%%%s' "$(combine_results "${logic_program}" "${time_limit}" | tr -d $'\n')" $'\t';
    )
    done
  ) | sed -E 's#\t$#\n#g' # Replacing last tab with newline
}

# List results for all programs (with .pl extension)
# associated with LPTP proof files (with .pr extension)
# in ./out directory.
#
# ```
# BEGIN{split("ackermann nat int gcd list suffix. revers. permut. sort merges. taut",coll);
# for(idx in coll) target_coll[coll[idx]]=idx}
# ```
# This is executed at the beginning before processing the file.
# It splits the string "ackermann nat int gcd list suffix reverse permut sort merge taut" into an array coll.
# Then, it creates a new associative array target_coll where the keys are the elements of coll
# and the values are their corresponding indices (idx).
#
# ```
# {print target_coll[$1], $0}:
# ```
#
# This is executed for each line in the file.
# It prints the index of the first field of the line ($1)
# in the custom order (retrieved from array target_coll),
# followed by the original line ($0).
#
# `cut` removes the first field (the index) from the output.
function list_all_results {
  print_results_header
  ls -1 ./src/*/*-all.pl | \
  sed -E 's#(-all)*\.pl$#-all.pl#g' | \
  sort | \
  uniq | \
  xargs -I{} bash \
    -c '. ./bin/parse_results.sh && join_parsed_results_for_predefined_time_limits "${1}"' \
    shell \
    {} | \
  awk 'BEGIN{split("ackermann nat int gcd list suffix. revers. permut. sort merges. taut",coll); for(idx in coll) target_coll[coll[idx]]=idx} {print target_coll[$1], $0}' - | \
  sort -k1,1n | \
  cut -d' ' -f2- | \
  tee ./out/results.txt
}

function list_all_results_latex {
  local output
  output="$(print_utf8 '%s' "$(
    print_results_header_latex
    list_all_results 2>&1 | \
    grep --invert-match 'lib' | \
    tr '[:blank:]' '_' | \
    sed -E 's#_# \&#g' | \
    sed -E 's/#/\\#/g' | \
    sed -E 's#%#\\%#g' | \
    sed -E 's#$#\\\\ \\hline#g' | \
    sort
    print_results_footer_latex
  )")"

  if command -v pbcopy >> /dev/null;
  then
    print_utf8 '%s' "${output}" | pbcopy
  fi

  print_utf8 '%s' "${output}" | \
  tee ./out/results.tex
}

set +Eeu
