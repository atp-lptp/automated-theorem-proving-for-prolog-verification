#!/usr/bin/env bash
set -Eeu

# To ask for confirmation by printing a message
#
# A confirmation message is passed as 1st argument
# When NO_INTERACTION environment variable is set to an non-empty string,
# choice is set to 'y' and no prompt is displayed.
#
# The user choice (keyboard input)
# is eventually written to standard output
function ask_for_confirmation {
  local choice

  local confirmation_message
  confirmation_message="${1}"

  if [ -z "${confirmation_message}" ];
  then
    print_optionally 'Missing confirmation message.'
    return 1
  fi

  if [ -z "${NO_INTERACTION}" ];
  then
    read -p "${confirmation_message} (y/n)?" choice
  else
    choice='y'
  fi
  unset NO_INTERACTION

  print_utf8 '%s' "${choice}"
}

# See [List of ANSI color escape sequences](https://stackoverflow.com/a/33206814)

# Print blue ANSI color escape sequence to standard output
# by assuming `echo` is one of [bash builtin commands](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-echo)
function _green_sequence {
    local sequence
    sequence="\033[32m"
    echo -ne "${sequence}"
}

# Print red ANSI color escape sequence to standard output
function _red_sequence {
    local sequence
    sequence="\033[31m"
    echo -ne "${sequence}"
}

# Print blue ANSI color escape sequence to standard output
function _blue_sequence {
    local sequence
    sequence="\033[34m"
    echo -ne "${sequence}"
}

# Print reset ANSI escape sequence to standard output
function _reset_sequence {
    local sequence
    sequence="\033[39m\\033[49m"
    echo -ne "${sequence}"
}

# Print newline to standard output
#
# Example:
#
# ```shell
# actual="$(_newline)";expected="$(print_utf8 $'\n')";message='_newline'
# ```
function _newline {
  print_utf8 '%s' $'\n'
}

# Print quoted newline to standard output
#
# Example:
#
# ```shell
# actual="$(_newline_char)";expected='\n';message='_newline_char'
# ```
function _newline_char {
  print_utf8 '%s' '\n'
}

# Print 1st argument to standard error
#
# Example:
#
# ```shell
# actual="$(print_to_stderr 'subject' 2>&1)";expected='subject';message='print_to_stderr'
# ```
function print_to_stderr {
  print_utf8 "${@}" 1>&2
}

# Print arguments to stderr
# when VERBOSE environment variable
# is set to an non-empty string.
function print_optionally {
  if [ -z "${VERBOSE}" ];
  then
    return
  fi

  print_to_stderr "${@}" 1>&2
}

# Print separator optionally
# and an optional suffix
#
# Example:
#
# ```shell
# actual="$(VERBOSE=1 print_separator ' ${SUFFIX}' 2>&1)";expected="$(print_utf8 '%s%s%s' $'\n' '------ ${SUFFIX}' $'\n')";message='print_separator'
# ```
function print_separator {
  local suffix
  suffix=''

  if [ -n "${1}" ];
  then
    suffix="${1}"
  fi

  print_optionally "$(_newline_char)"'%s%s'"$(_newline_char)" '------' "${suffix}"
}

# Print to standard error that
# file located at filepath passed as 1st argument
# has been overwritten
#
# Example:
#
# ```shell
# actual="$(VERBOSE=1 print_subject_is_overwritten './src/nat/nat-all.pr' 2>&1)";expected="$(print_utf8 '%s%s%s%s' "$(_blue_sequence)" "'./src/nat/nat-all.pr' has been overwritten." $'\n' "$(_reset_sequence)")";message='print_subject_is_overwritten'
# ```
function print_subject_is_overwritten {
  print_in_blue "'${1}' has been overwritten."
}

# Expand "$@" and pass the expansion to printf
# > "$@" is equivalent to "$1" "$2" …
# See [special parameters](https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html)
#
# redirect what print_utf8 writes to standard output to standard error,
# and print a newline and reset sequence to standard error
function print_args_before_reset_sequence {
  print_to_stderr "$(print_utf8 "$@";_newline;_reset_sequence)"
}

# Print red color sequence to standard error
# before calling `print_args_before_reset_sequence`
function print_in_red {
  print_to_stderr "$(_red_sequence)"
  print_args_before_reset_sequence "${@}"
}

# Print green color sequence to standard error
# before calling `print_args_before_reset_sequence`
function print_in_green {
  if [ -z "${VERBOSE}" ];
  then
    return
  fi

  print_to_stderr "$(_green_sequence)"
  print_args_before_reset_sequence "${@}"
}

# Print blue color sequence to standard error
# before calling `print_args_before_reset_sequence`
function print_in_blue {
  if [ -z "${VERBOSE}" ];
  then
    return
  fi

  print_to_stderr "$(_blue_sequence)"
  print_args_before_reset_sequence "${@}"
}

# Print results header
function print_results_header {
  print_utf8 '%s'$'\t''%s'$'\t' 'lib' '#'
  print_utf8 '%s'$'\t''%s'$'\t''%s'$'\t' 'E-1s' 'V-1s' 'EV-1s'
  print_utf8 '%s'$'\t''%s'$'\t''%s'$'\t' 'E-10s' 'V-10s' 'EV-10s'
  print_utf8 '%s'$'\t''%s'$'\t''%s'$'\t' 'E-60s' 'V-60s' 'EV-60s'
  _newline
}

# Print LaTeX header
function print_results_header_latex {
    print_utf8 '%s' "$(
        cat <<-'LaTeX'
\begin{table}
    \centering
    \begin{tabular}{|l|l|l|c|c|c|l|c|c|c|l|c|c|c|} \hline
    \textit{lib}&{\#}&E-1s&  V-1s&EV-1s&  E-10s&  V-10s&  EV-10s&  E-60s&  V-60s& EV-60s\\ \hline
LaTeX
    )"
    _newline
}

# Print LaTeX footer
function print_results_footer_latex {
    print_utf8 '%s' "$(
        cat <<-'LaTeX'
    \end{tabular}
    \caption{Success rate}
    \label{tab:success_rate}
\end{table}
LaTeX
    )"
    _newline
}

# Call print_utf8 with quoted expanded argument list
# after setting locale environment variables to "en_US.UTF-8" by default
#
# Example:
#
# ```shell
# actual="$(print_utf8 '%.0f' '45.4')";expected="45";message='print_utf8'
# actual="$(print_utf8 '%.0f' '45.5')";expected="46";message='print_utf8'
# actual="$(print_utf8 '%.0f' '45')";expected="45";message='print_utf8'
# ```
#
# See also [printf command](https://www.ibm.com/docs/en/aix/7.2?topic=p-printf-command)
function print_utf8 {
  (
    local print_locale
    print_locale="${PRINT_LOCALE:-en_US.UTF-8}"

    export LC_IDENTIFICATION="${print_locale}"
    export LC_MEASUREMENT="${print_locale}"
    export LC_NUMERIC="${print_locale}"
    export LC_ALL="${print_locale}"
    export LANG="${print_locale}"
    export LC_NUMERIC="${print_locale}"
    printf "$@"
  )
}

set +Eeu