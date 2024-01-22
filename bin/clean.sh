#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/print.sh'

# Remove lock file created by 'list_requirements' function
# lock files are created in ./out directory
# to prevent listing requirements more than once for the same logic program.
function remove_lock_files {
  if [ $(ls -1 ./out/*_DIR_SEP_* 2> /dev/null | wc -l) -eq 0 ];
  then
    return
  fi

  find ./out/*_DIR_SEP_* -exec bash -c 'rm -v ${1} >> /dev/null 2>&1' shell {} \;
}

# Ask for confirmation before removing output files (with '.out' extension)
# in ./out/ directory if there are any.
function clean_files_ending_with_out_extension {
  local list_output_files
  list_output_files='cd ./out && find ./*\.out'

  local how_many_output_files
  how_many_output_files=$(bash -c "${list_output_files}" 2> /dev/null | wc -l)

  if [ ${how_many_output_files} -gt 0 ];
  then
    local confirmation_message
    confirmation_message="$(
      print_in_red "Do you confirm deletion of %d output files?%s" "${how_many_output_files}"
      bash -c "${list_output_files}" 2> /dev/null
      print_utf8 '%s' $'\n'
    )"

    local choice
    choice="$(ask_for_confirmation "${confirmation_message}")"

    case "${choice}" in
      y|Y )
        bash -c "${list_output_files}" 2> /dev/null | \
        xargs -I{} bash -c 'rm ./out/${1} && echo "${1} removed successfully." 1>&2' shell {}
      ;;
      n|N ) echo 'no';;
      * ) echo 'invalid answer';;
    esac

  else
    print_utf8 '%s.%s' 'No output files to clean' $'\n' 1>&1
  fi
}

# Ask for confirmation before removing output files in ./src subdirectories
# having filename ending with
# - '-all.pl',
# - '-all.pr',
# - '-all*.fof',
# if any.
function clean_files_ending_gr_pl_pr {
  local list_output_files
  list_output_files='cd ./src && find ./*/*-all.pl ./*/*-all.pr ./*/*-all.gr ./*/*-all*.fof'

  local how_many_output_files
  how_many_output_files=$(bash -c "${list_output_files}" 2> /dev/null | wc -l)

  if [ ${how_many_output_files} -gt 0 ];
  then
    local confirmation_message
    confirmation_message="$(
      print_in_red "Do you confirm deletion of %d output files?%s" "${how_many_output_files}"
      bash -c "${list_output_files}" 2> /dev/null
      print_utf8 '%s' $'\n'
    )"

    local choice
    choice="$(ask_for_confirmation "${confirmation_message}")"

    case "${choice}" in
      y|Y )
        bash -c "${list_output_files}" 2> /dev/null | \
        xargs -I{} bash -c 'rm ./src/${1} && echo "${1} removed successfully." 1>&2' shell {}
      ;;
      n|N ) echo 'no';;
      * ) echo 'invalid answer';;
    esac

  else
    print_utf8 '%s.%s' 'No output files to clean' $'\n' 1>&1
  fi
}

# Ask for confirmation before removing output files
# having filename ending with
# - '-all.pl' in ./src subdirectories,
# - '-all.pr' in ./src subdirectories,
# - '.out' in ./out/ directory
# if any.
function clean {
  clean_files_ending_gr_pl_pr
  clean_files_ending_with_out_extension
}

set +Eeu
