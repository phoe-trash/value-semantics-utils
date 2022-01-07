# VALUE-SEMANTICS-UTILS

Utilities for adjusting CLOS for mostly-functional programming. Includes:

* `eqv`, an equivalence predicate that acts mostly like `equal`
  except it is extensible and does not hang on cycles;
* `eqv-using-class`, a means of programming `eqv`;
* `class-with-value-semantics`, a metaclass which automatically adds
  `eqv-using-class` methods specialized on the class being defined;
* `always-bound-class`, a metaclass whose instances cannot have their
  slots unbound at any time;
* `typechecked-class`, an `always-bound-class` with mandatory runtime
  typechecking for slot values;
  * `typechecked-slot-definition` and `typechecked-effective-slot-definition`,
    slot definition classes for typechecked slots;
  * `slot-definition-typecheck-function`, an accessor function for the typecheck
    function of a typechecked slot;
* `typechecked-class-with-value-semantics`, a composition of the above three
  metaclasses.

## TODO

* Rationale and examples and manual
* Docstrings
* Wait for https://bugs.launchpad.net/sbcl/+bug/1956621 to get fixed and
  unskip a test that depends on it

## License

MIT.

