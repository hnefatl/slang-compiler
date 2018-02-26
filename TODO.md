# TODO

A list of random thoughts to be implemented.

## Major

- [x] Interpreter 0
- [ ] Interpreter 1
- [ ] Interpreter 2
- [ ] Interpreter 3
- [ ] Jargon VM

## Minor

- [x] Rewrite the underlying monad for Alex and Happy.
  - [x] Hook it into the typechecker and interpreters to allow for error messages dependent on location.
- [x] Error messages - make a typeclass for "error messages" and ADT instances of it for each thing that can have errors. Report errors as an item from the relevant ADT, do the string conversion later.
  - [ ] Add more context to the error messages - allow for passing more relevant information.
- [ ] Differentiate between arbitrary precision integers and fixed precision.
  - [ ] Make changes to low-level stack structure as necessary.
- [ ] Add optional nicer syntax - eg. remove the "end" from "while" loops.
- [ ] Split away from Slang - new name, something like Slang++ or Dialect.