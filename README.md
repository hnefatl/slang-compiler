# SLang Compiler

[![Build Status](https://travis-ci.org/hnefatl/slang-compiler.svg?branch=master)](https://travis-ci.org/hnefatl/slang-compiler)

A simple compiler for the SLang language, as used in the Part 1B "Compiler Construction" module.

This repository follows the standard Stack layout:

- `src/` contains the "library" code (the API).
- `app/` contains the "application" code (the runnable compiler).
- `test/` contains the test suite.

The Lexer is generated using [Alex](https://www.haskell.org/alex/), and the Parser is generated using [Happy](https://www.haskell.org/happy/).

Tests are written using [`Tasty`](https://github.com/feuerbach/tasty) with the `HUnit` and `QuickCheck` providers.