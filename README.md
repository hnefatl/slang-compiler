# SLang Compiler

A simple compiler for the SLang language, as used in the Part 1B "Compiler Construction" module.

This repository follows the standard Stack layout:

- `src/` contains the "library" code (the API).
- `app/` contains the "application" code (the runnable compiler).
- `test/` contains the test suite.

Tests are written using [`Tasty`](https://github.com/feuerbach/tasty) with the `HUnit` and `QuickCheck` providers.