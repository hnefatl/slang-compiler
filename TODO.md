# TODO

A list of random thoughts to be implemented.

- Rewrite the underlying monad for Alex and Happy.
  - Hook it into the typechecker and interpreters to allow for error messages dependent on location.
- Error messages - make a typeclass for "error messages" and ADT instances of it for each thing that can have errors. Report errors as an item from the relevant ADT, do the string conversion later.
- Differentiate between arbitrary precision integers and fixed precision.
  - Make changes to low-level stack structure as necessary.
- Add optional nicer syntax - eg. remove the "end" from "while" loops.
- Split away from Slang - new name, something like Slang++ or Dialect.