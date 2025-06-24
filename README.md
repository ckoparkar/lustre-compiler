## lustre-compiler

Prototype compiler for the [Lustre](https://www-verimag.imag.fr/The-Lustre-Programming-Language-and) language.
References:
- [A Formally Verified Compiler for Lustre](https://inria.hal.science/hal-01512286)
- [Mechanized semantics and verified compilation for a dataflow synchronous language with reset](https://dl.acm.org/doi/10.1145/3371112)
- [Weaving Synchronous Reactions into the Fabric of SSA-form Compilers](https://dl.acm.org/doi/full/10.1145/3506706)
- [Verified Lustre Normalization with Node Subsampling](https://dl.acm.org/doi/10.1145/3477041)


### Dependencies

This project is written in Haskell and thus depends on GHC and Cabal
which can be installed using [GHCup](https://www.haskell.org/ghcup/).

### Usage

After cloning this repository you can run:

```
$ git submodule update --init
$ cabal v2-build all
$ cabal v2-exec lustre-compiler -- -v3 --prog=examples/test2.lus --log=dbg.log
Writing logs to HOME/lustre-compiler/dbg.log.
Writing C code to HOME/lustre-compiler/examples/test2.lus.compiled/test2.c.
```

### License

Copyright (c) 2025, Chaitanya Koparkar

All rights reserved.
