#!/usr/bin/env bash
#
# Non-regression tests for src/indp.pl
#
# For each <program>.pl / <program>.pr pair listed in expected.txt:
#   - generate the .fof files with filename_fofs/1, in a temporary directory
#     (nothing is written in the repository),
#   - count the induction axioms of each .fof file,
#   - apply E and Vampire to it,
#   - compare both with expected.txt.
#
# Usage: ./test/indp/run.sh            (or: make test-indp)
#        TIME_LIMIT=60 ./test/indp/run.sh
#
# Exit status: 0 when every check passes, 1 otherwise.

set -Eeu

test_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root_dir="$(cd "${test_dir}/../.." && pwd)"
time_limit="${TIME_LIMIT:-10}"

for cmd in swipl eprover vampire; do
  if ! command -v "${cmd}" > /dev/null 2>&1; then
    printf 'Error: %s is required\n' "${cmd}" 1>&2
    exit 1
  fi
done

tmp_dir="$(mktemp -d)"
trap 'rm -rf "${tmp_dir}"' EXIT

# prover_status(+Prover,+FofFile): print Theorem or Unproved
prover_status() {
  local output
  case "${1}" in
    E)       output="$(eprover --auto --cpu-limit="${time_limit}" -s "${2}" 2>&1 || true)" ;;
    Vampire) output="$(vampire --proof off --time_limit "${time_limit}s" "${2}" 2>&1 || true)" ;;
  esac
  if grep -q 'SZS status Theorem' <<< "${output}"; then
    echo 'Theorem'
  else
    echo 'Unproved'
  fi
}

# Generate the .fof files of each program once
programs="$(grep -v -e '^#' -e '^[[:space:]]*$' "${test_dir}/expected.txt" | awk '{print $1}' | sort -u)"
for program in ${programs}; do
  mkdir -p "${tmp_dir}/$(dirname "${program}")"
  cp "${test_dir}/${program}.pl" "${test_dir}/${program}.pr" "${tmp_dir}/$(dirname "${program}")/"
  (cd "${root_dir}/src" && \
    swipl -q -g "consult('prove.pl')" -g "filename_fofs('${tmp_dir}/${program}')" -t halt > /dev/null)
done

passed=0
failed=0
while read -r program lemma expected_inductions expected_status; do
  fof="${tmp_dir}/${program}${lemma}.fof"
  label="${program} lemma ${lemma}"
  if [ ! -f "${fof}" ]; then
    printf 'FAILURE %s: %s not generated\n' "${label}" "$(basename "${fof}")"
    failed=$((failed + 1))
    continue
  fi
  inductions="$(grep -c '^fof(induction,' "${fof}" || true)"
  e_status="$(prover_status E "${fof}")"
  v_status="$(prover_status Vampire "${fof}")"
  if [ "${inductions}" = "${expected_inductions}" ] && \
     [ "${e_status}" = "${expected_status}" ] && \
     [ "${v_status}" = "${expected_status}" ]; then
    printf 'OK      %-28s induction axioms: %s, E: %s, Vampire: %s\n' \
      "${label}" "${inductions}" "${e_status}" "${v_status}"
    passed=$((passed + 1))
  else
    printf 'FAILURE %-28s induction axioms: %s (expected %s), E: %s, Vampire: %s (expected %s)\n' \
      "${label}" "${inductions}" "${expected_inductions}" "${e_status}" "${v_status}" "${expected_status}"
    failed=$((failed + 1))
  fi
done < <(grep -v -e '^#' -e '^[[:space:]]*$' "${test_dir}/expected.txt")

printf '%s passed, %s failed (time limit: %ss)\n' "${passed}" "${failed}" "${time_limit}"
[ "${failed}" -eq 0 ]
