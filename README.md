# QGAT: A Generate-and-Test Paradigm for Quantum Circuits

This is a DSL for certain classes of quantum algorithms embedded in Haskell, first presented at [PLanQC 2024][planqc24].
See the extended abstract to be posted there for more information.

It is built on [Quipper][quipper].
Currently it relies on [a fork][quipper-fork] which has fixes to make it work with newer versions of GHC (tested with 9.6.2), and additional features required by QGAT.
This repository has the fork as a Git submodule, so clone it with:
```
git clone --recursive https://github.com/ulrikdem/qgat
```

The main implementation is in `QGAT.hs`.
The `examples` directory has implementations of the Deutsch-Jozsa, Bernstein-Vazirani, Simon and Shor algorithms.
The examples are configured in the Cabal package, and can be run with, for example:
```
cabal run BernsteinVazirani
```

[planqc24]: https://popl24.sigplan.org/details/planqc-2024-papers/14/QGAT-A-Generate-and-Test-Paradigm-for-Quantum-Circuits
[quipper]: https://www.mathstat.dal.ca/~selinger/quipper/
[quipper-fork]: https://github.com/ulrikdem/quipper
