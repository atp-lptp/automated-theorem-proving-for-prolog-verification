#!/usr/bin/env bash
set -Eeu

# Assuming this script is sourced from the root directory.
. './bin/check.sh'
. './bin/clean.sh'
. './bin/print.sh'
. './bin/rename.sh'

# Count how many times 'needs_gr' or 'needs_thm'
# is found in a LPTP proof file (with .pr extension)
# associated with logic program (with .pl extension) passed as 1st argument.
#
# Occurrences matching the original logic program name are ignored.
#
# For instance, having a LPTP proof file named 'src/nat/nat.pr'
# associated to 1st argument './src/list/list.pl'
# passed as 1st argument with 'src/nat/nat.pr' containing
# ```prolog
# % [..]
# :- needs_gr($(lib)/list/list).
# :- needs_gr($(lib)/nat/nat).
# :- needs_thm($(lib)/nat/nat).
# % [..]
# ```
# 2 requirement are counted
# (by ignoring the first one, matching the original program basename, "list").
#
# ```shell
# actual=$(count_requirements "./src/list/list.pl");expected=2;message='count_requirements'
# actual=$(count_requirements "./src/sort/mergesort.pl");expected=6;message='count_requirements'
# ```
function count_requirements {
    local logic_program
    logic_program="${1}"

    if ! guard_against_invalid_logic_program "${logic_program}"; then
        return 1
    fi

    local requirement_pattern
    requirement_pattern='needs_[gr\|thm]'

    # - search for lines containing needs_gr or needs_thm
    # - ignore lines ending with original program name without suffix
    # - count lines
    grep --recursive "${requirement_pattern}" "$(program_to_proof "${logic_program}")" |
        grep --invert-match "$(without_pl_suffix "${logic_program}"))\." |
        grep --count ''
}

# List recursively requirements (with .gr and .thm extensions)
# required by a LPTP proof file (with .pr extension)
# associated to a logic program (with .pl extension)
# passed as 1st argument
# (excluding .gr file associated to the 1st argument).
#
# Example:
#
# ```shell
# actual=$(list_requirements "./src/ackermann/ackermann.pl" | \grep -c "nat.thm");expected=0;message='list_requirements'
# actual=$(list_requirements "./src/ackermann/ackermann.pl" | \grep -c "nat.gr");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "list.gr");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "list.thm");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "permutation.gr");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "permutation.thm");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "nat.thm");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -c "nat.gr");expected=1;message='list_requirements'
# actual=$(list_requirements "./src/sort/mergesort.pl" | \grep -cE ".(gr|thm)$");expected=6;message='list_requirements'
# actual="$(list_requirements "./src/nat/nat.pl")";expected='';message='list_requirements'
# actual=$(list_requirements "./src/nat/nat-all.pl");expected='src/nat/nat.gr';message='list_requirements'
# ```
#
# ```shell
# list_requirements ./src/sort/mergesort.pl
# ```
# ```stdout
# src/list/list.gr
# src/list/list.thm
# src/nat/nat.gr
# src/nat/nat.thm
# src/list/permutation.gr
# src/list/permutation.thm
# ````
function list_requirements {
    local logic_program
    logic_program="${1}"

    if ! guard_against_invalid_logic_program "${logic_program}"; then
        return 1
    fi

    local proof_file
    proof_file="$(program_to_proof "${logic_program}")"

    local exclusion_pattern
    exclusion_pattern="${2}"

    if [ -z "${exclusion_pattern}" ]; then
        exclusion_pattern="/$(without_pl_suffix "${logic_program}")\)"
    else
        exclusion_pattern="${exclusion_pattern}|/$(without_pl_suffix "${logic_program}")\)"
    fi

    print_separator "START '${logic_program}' requirements"

    local total_requirements
    total_requirements=$(count_requirements "${logic_program}")

    local lock_file
    lock_file="$(program_to_lock_file "${logic_program}")"

    if [ -e "${lock_file}" ]; then
        return
    fi

    if [ ${total_requirements} -eq 0 ]; then
        print_in_blue 'No requirements found.'
    else
        local requirement_pattern
        requirement_pattern='needs_[gr\|thm]'

        local substitution_pattern
        substitution_pattern='s#.+needs_(gr|thm)\(\$\(lib\)([^)]+)\).#src\2.\1#'

        local requirements
        requirements="$(grep --no-filename --recursive "${requirement_pattern}" "${proof_file}" |
            grep --invert-match --extended-regexp "${exclusion_pattern}" |
            sed -E "${substitution_pattern}" |
            sort |
            uniq |
            sed -E 's#\n$##g' |
            tee "${lock_file}")"

        print_utf8 '%s' "${requirements}"

        print_in_blue '%s%s%s%s' $'\n' $'\n' $'\n' $'\n'
        print_in_blue '[list_requirements] ${exclusion_pattern}: '"'"'%s'"'"'%s' "${exclusion_pattern}"
        print_in_blue '[list_requirements] ${proof_file}: '"'"'%s'"'"'%s' "${proof_file}"

        if [ -n "${requirements}" ]; then
            print_in_blue '[list_requirements] ${requirements}: %s```%s%s%s```%s' $'\n' $'\n' "${requirements}" $'\n'
        else
            print_in_blue '[list_requirements] ${requirements}: '"'"''"'"'%s' $'\n'
        fi
    fi

    print_separator "END   '${logic_program}' requirements"
}

# Given a logic program passed as 1st argument (with .pl extension),
# 'build_pl' function concatenates all requirements (with .pl extensions)
# required by the associated LPTP proof file (with .pr extension)
# before writing the concatenation result in a target file
# prefixed with original logic program basename and
# suffixed with
# - '-all.pl'
#
# Example:
#
# ```shell
# NO_INTERACTION=1 build_pl './src/sort/mergesort.pl'
# ```
#
# will concatenate
# - `./src/sort/mergesort.pl src/list/list.pl src/nat/nat.pl src/permutation/permutation.pl`
# onto `./src/sort/mergesort-all.pl`.
#
# Unsetting NO_INTERACTION would prompt for confirmation before overriding target files.
function build_pl {
    local logic_program
    logic_program="${1}"

    if ! guard_against_invalid_logic_program "${logic_program}"; then
        return 1
    fi

    local depth
    depth=${2}

    if [ -z "${depth}" ]; then
        depth=0
        remove_lock_files
    fi

    local exclusion_pattern
    exclusion_pattern="${3}"

    if [ -z "${exclusion_pattern}" ]; then
        exclusion_pattern='^$'
    fi

    local sub_requirements
    local d

    local requirements
    requirements="$(for d in $(list_requirements "${logic_program}" "${exclusion_pattern}" | from_gr_thm_to_pl | sort | uniq); do
        print_in_blue '%s' $'\n'
        print_in_blue '[build_pl_gr] ${exclusion_pattern}: %s' "${exclusion_pattern}"
        exclusion_pattern="${exclusion_pattern}|/$(without_pl_suffix "${d}")\)"
        sub_requirements="$(build_pl "${d}" $((depth + 1)) "${exclusion_pattern}")"
        if [ -n "${sub_requirements}" ]; then
            print_utf8 '%s%s' "${sub_requirements}" $'\n'
        fi
    done |
        sort |
        uniq)"

    if [ ${depth} -gt 0 ]; then
        print_utf8 '%s%s' "${logic_program}" $'\n'
        if [ -n "${requirements}" ]; then
            print_utf8 '%s%s' "${requirements}" $'\n'
        fi

        return
    fi

    local pl_target_file
    pl_target_file="${logic_program//.pl/-all.pl}"

    if [ ${depth} -eq 0 ]; then
        local choice
        choice="$(ask_for_confirmation 'Do you confirm target file override? ("'"${pl_target_file}"'")')"

        case "$choice" in
        y | Y)
            local without_line_breaks
            without_line_breaks="$(print_utf8 '%s%s' "${logic_program} ${requirements}" $'\n' | sed 's# #\n#g')"

            print_utf8 '%s' "${without_line_breaks}" |
                xargs -I{} bash -c '\cat ${1};echo;echo;' shell {} \
                    >"${pl_target_file}"
            print_in_blue '%s%s' "'${pl_target_file}' has been overwritten."
            ;;
        n | N) print_utf8 '%s%s' 'no' '%s' ;;
        *) print_utf8 '%s%s' 'invalid' '%s' ;;
        esac

        remove_lock_files
    fi
}

set +Eeu
