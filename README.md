# Automated Theorem Proving for Prolog Verification

## Build FOF files

To generate the FOF files corresponding to a logic program and its associated proof file,  
please enter the following commands in the terminal.

```shell
export LOGIC_PROGRAM='./src/nat/nat.pl'
# generate ./src/nat/nat-all*.fof
make build-fof 
```

## Apply provers

Then to apply both the *E Theorem Prover* and *Vampire* for each predefined limit (`1s`,`10s`,`60s`),  
please enter the following commands in the terminal.

```shell
export LOGIC_PROGRAM='./src/nat/nat-all.pl'
make start
```

The same procedure can be applied to the other available logic programs:
- ./src/ackermann/ackermann-all.pl
- ./src/gcd/gcd-all.pl
- ./src/int/int-all.pl
- ./src/list/list-all.pl
- ./src/permutation/permutation-all.pl
- ./src/reverse/reverse-all.pl
- ./src/sort/mergesort-all.pl
- ./src/sort/sort-all.pl
- ./src/suffix/suffix-all.pl
- ./src/taut/taut-all.pl

## Requirements

- macOS, Linux-based operating systems  
  tested with
    - debian *Bookmworm*,
    - macos *Sonoma 14.21*
- *E Theorem Prover* [^1]
- *Vampire* [^2]
- SWI-Prolog [^3]
    - to build FOF (*first-order form*) files
- GNU Make [^4]
- GNU Bash [^5]
- GNU bc [^6]
    - to parse results (in `./out` directory)

## Help

Output inline commands documentation.

```shell
make help
```

## License

See [License](./COPYING)

## Copyright

Copyright (C) 2024-...  
Thierry Marianne -- thierry.marianne@univ-reunion.fr  
Fred Mesnard -- frederic.mesnard@univ-reunion.fr  
Etienne Payet -- etienne.payet@univ-reunion.fr  

[^1]: https://wwwlehre.dhbw-stuttgart.de/~sschulz/E/Download.html
[^2]: https://vprover.github.io/download.html
[^3]: https://www.swi-prolog.org/Download.html
[^4]: https://www.gnu.org/software/make/#download
[^5]: https://www.gnu.org/software/bash/
[^6]: https://www.gnu.org/software/bc/
