SHELL := /bin/bash

SUFFIX_E_PROVER ?= "_e_prover"
SUFFIX_VAMPIRE ?= "_vampire"

# Apply both Vampire system and E Theorem Prover in parallel
MAKEFLAGS += -j2

.PHONY: \
	apply-e-theorem-prover \
	apply-vampire \
	apply-provers \
	all-results \
	build-fof \
	check \
	help \
	results \
	start

help: ### Show available commands short description
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

check-requirements: ### Check if requirements are satisfied
	@. ./bin/check.sh && \
	check_requirements "$${LOGIC_PROGRAM}"

build-fof: ### Build .fof files (conjectures and axioms in first-order form) for logic program exported as LOGIC_PROGRAM environment variable
	@. ./bin/start.sh && \
	build_fof "$${LOGIC_PROGRAM}"

stop: ### Stop running prover.pl
	. ./bin/start.sh && \
	stop

apply-e-theorem-prover: check-requirements ### Apply E Theorem Prover to conjectures and axioms in first-order form
	@export \
	PROVER_CMD_TPL="$$(which eprover)"' --auto --cpu-limit=_TIMEOUT_ -s ' \
	OUTPUT_SUFFIX="${SUFFIX_E_PROVER}" && \
	. ./bin/start.sh && \
	apply_prover_with_predefined_time_limits "$${PROVER_CMD_TPL}" "$${OUTPUT_SUFFIX}" "$${LOGIC_PROGRAM}"

apply-vampire: check-requirements ### Apply Vampire system to conjectures and axioms in first-order form
	@export \
	PROVER_CMD_TPL="$$(which vampire)"' --proof off --time_limit _TIMEOUT_s ' \
	OUTPUT_SUFFIX="${SUFFIX_VAMPIRE}" && \
	. ./bin/start.sh && \
	apply_prover_with_predefined_time_limits "$${PROVER_CMD_TPL}" "$${OUTPUT_SUFFIX}" "$${LOGIC_PROGRAM}"

apply-provers: apply-vampire apply-e-theorem-prover ### Apply both Vampire system and E Theorem Prover to conjectures and axioms in first-order form

start: apply-vampire apply-e-theorem-prover

apply-provers-for-each-program: ### Apply both Vampire system and E Theorem Prover to conjectures and axioms in first-order form for each program in ./src
	. ./bin/start.sh && \
	apply_prover_for_each_program_with_predefined_time_limits

build:
	### Build .fof,.pl,.pr files for logic program exported as LOGIC_PROGRAM environment variable
	@. ./bin/build.sh && \
	build "$${LOGIC_PROGRAM}"

build-prover-image:
	### Build prover image
	. ./bin/docker.sh && \
	build_prover_image

run-prover-container:
	### Run prover container
	. ./bin/docker.sh && \
	run_prover_container

build-requirements-for-each-program:
	### Build .fof,.pl,.pr files for all logic programs available in ./src
	@. ./bin/build.sh && \
	build_all

build-fof-for-each-program: ### Build all .fof files for all logic programs available in ./src
	@. ./bin/build.sh && \
	build_all_fof

results:
 	### Parse results in ./out for "${LOGIC_PROGRAM}" exported as environment variable
	@export \
 	SUFFIX_E_PROVER="${SUFFIX_E_PROVER}" \
 	SUFFIX_VAMPIRE="${SUFFIX_VAMPIRE}" && \
	. ./bin/parse_results.sh && \
	print_results_header && \
	join_parsed_results_for_predefined_time_limits "$${LOGIC_PROGRAM}"

results-for-each-program:
 	### Parse all results in ./out
	@export \
 	SUFFIX_E_PROVER="${SUFFIX_E_PROVER}" \
 	SUFFIX_VAMPIRE="${SUFFIX_VAMPIRE}" && \
	. ./bin/parse_results.sh && \
	list_all_results

all-results: results-for-each-program

all-results-in-latex: ### Parse all results in ./out before rendering them in LaTeX format
	@export \
 	SUFFIX_E_PROVER="${SUFFIX_E_PROVER}" \
 	SUFFIX_VAMPIRE="${SUFFIX_VAMPIRE}" && \
	. ./bin/parse_results.sh && \
	list_all_results_latex

test: ### Build tests from examples provided in comments before running them
	. ./bin/build.sh && \
	run_tests