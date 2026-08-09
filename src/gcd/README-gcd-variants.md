# Two variants of `gcd`, side by side

`gcd.pl` and `gcd_ite.pl` define the same relation with two different Prolog
idioms. Both are kept on purpose: they behave identically at run time, but they
do not produce the same completion, and the provers do not behave the same on
them.

| | program | style |
|---|---|---|
| `gcd.pl` / `gcd-all.pl` | default | two clauses per predicate, guards `X @=< Y` / `Y @< X`, structural match `0` / `s(X)` |
| `gcd_ite.pl` / `gcd_ite-all.pl` | variant | one clause per predicate, ISO `( Cond -> Then ; Else )` |

The two `-all.pl` files are identical apart from the `gcd/3` and `gcd_leq/3`
block; both carry the same `nat`, `plus` and `@=<` definitions, and both use the
same 11 conjectures (`gcd-all.pr` and `gcd_ite-all.pr` have the same content).

## They denote the same relation

Checked exhaustively over `0..10 x 0..10` (121 pairs, including the cases where
one argument is `0`): the two versions return the same set of solutions for
every pair. The guards are nevertheless *not* syntactically the same — the
if-then-else compiles its else-branch to `~ (X @=< Y)` rather than `Y @< X`, and
`gcd_leq` tests `X = 0` rather than matching `0` against `s(X)`.

## They do not produce the same completion

`indp.pl` translates a body `( C -> T ; E )` into `(C & T) \/ (~ C & E)`, which
is what LPTP itself does (see `lptp/src/gnd.pl`, `gnd__disjunction/5`).

Two clauses (`gcd-all1.fof`, verbatim modulo variable names):

    gcd_succeeds(X1,X2,X3) <=> ('@<_succeeds'(X2,X1)  & gcd_leq_succeeds(X2,X1,X3))
                             | ('@=<_succeeds'(X1,X2) & gcd_leq_succeeds(X1,X2,X3))

    gcd_leq_succeeds(X1,X2,X3) <=> ? [X4,X5] : (X1 = s(X4) & (plus_succeeds(s(X4),X5,X2)
                                                              & gcd_succeeds(s(X4),X5,X3)))
                                 | (X1 = '0' & X3 = X2)

If-then-else (`gcd_ite-all1.fof`, verbatim modulo variable names):

    gcd_succeeds(X1,X2,X3) <=> ('@=<_succeeds'(X1,X2) & gcd_leq_succeeds(X1,X2,X3))
                             | ('@=<_fails'(X1,X2)   & gcd_leq_succeeds(X2,X1,X3))

    gcd_leq_succeeds(X1,X2,X3) <=> ? [X4] : ((X1 = '0' & X3 = X2)
                                           | (~ (X1 = '0') & (plus_succeeds(X1,X4,X2)
                                                              & gcd_succeeds(X1,X4,X3))))

The variant brings in `'@=<_fails'` and a negated equality `~ (X = '0')` where
the default version had a positive `'@<_succeeds'` and a structural `s(...)`
pattern. That is the whole difference, and it is enough to change the search.

## Prover results

Same conjectures, same axioms apart from the completion above.

At a **10 s** limit, both provers, all 11 conjectures:

| | E | Vampire |
|---|---|---|
| `gcd` (two clauses) | 5 / 11 | 5 / 11 |
| `gcd_ite` | 5 / 11 | **4 / 11** |

The one that flips is `theorem-(gcd:complete)` (`*-all9.fof`), and it is a
slowdown rather than a loss — measured on the same machine:

| | Vampire on `*-all9.fof` |
|---|---|
| `gcd` (two clauses) | Theorem in **1 s** |
| `gcd_ite` | Theorem in **39 s** |

At a 60 s limit `gcd_ite` is back to 5 / 11 for both provers. E is not affected:
it proves all five in 1 s or less on either version.

The six conjectures that fail are the same set in both variants at 10 s, and
still the same set for `gcd_ite` at 60 s: `(div):plus`, `(div):minus`,
`gcd:types`, `gcd:(div)`, `gcd:complete`, `gcd:termination` — the standalone
`lemma-` ones. They go through once the preceding results are available as
axioms, which is why the `theorem-` and `corollary-` forms succeed. (The
two-clause version was not re-measured at 60 s: each run costs about 13 minutes,
almost all of it spent burning the budget on those six.)

## Reproducing

    export LOGIC_PROGRAM='./src/gcd/gcd-all.pl'      # or gcd_ite-all.pl
    make start

To regenerate the FOF files after editing either program:

    ./src/indp.sh src/gcd/gcd_ite-all

## Practical note

The default limits used by `make start` are 1 s, 10 s and 60 s. The two-clause
version is the one to keep as the default: it costs nothing at 1 s where the
variant needs 39 s for the same theorem.

`gcd_ite.pr` is a copy of `gcd.pr` (same conjectures, only `tex_file` renamed).
It is wired for this repository's FOF pipeline, which reads the `.pl` for the
completion and the `.pr` for the conjecture statements only. Re-checking the
variant with LPTP itself would additionally require a ground representation
matching the if-then-else clauses; `gcd.pr` currently pulls in the library one
via `needs_gr($(lib)/nat/gcd)`.
