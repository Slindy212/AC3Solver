## Module organization

AC3Solver.hs - Baseline AC3 Solver with test cases for small compact AC3 algorithms

Soduku.hs - Expansion of the AC3 Solver algorithm to a larger more understandable problem

## Building, running, and testing

Test cases at the end of each function

Larger end-to-end testing for the 3x3 Soduku boards at the end of Soduku.hs

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

