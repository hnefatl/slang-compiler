# SLang Compiler

[![Build Status](https://travis-ci.org/hnefatl/slang-compiler.svg?branch=master)](https://travis-ci.org/hnefatl/slang-compiler)

A simple compiler for the SLang language, as used in the Part 1B "Compiler Construction" module.

This repository follows the standard Stack layout:

- `src/` contains the "library" code (the API).
- `app/` contains the "application" code (the runnable compiler).
- `test/` contains the test suite.

The Lexer is generated using [Alex](https://www.haskell.org/alex/), and the Parser is generated using [Happy](https://www.haskell.org/happy/) (they're glued together through some StackOverflow-fuelled black-magic<sup>[1](https://stackoverflow.com/questions/20315739/how-to-use-an-alex-monadic-lexer-with-happy), [2](https://stackoverflow.com/questions/31996489/what-causes-happy-to-throw-a-parse-error)</sup>, I'm hoping to completely rewrite the glue at some point to make it clearer what's going on).

Tests are written using [`Tasty`](https://github.com/feuerbach/tasty) with the `HUnit` and `QuickCheck` providers.