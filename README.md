# VALUE-SEMANTICS-UTILS

Utilities for using CLOS for mostly-functional programming.

## Why?

There are two worlds in programming: an imperative one, full of objects and comparisons by identity, and a functional one, full of referential transparency and comparisons by value. Common Lisp is good at both, and I ended up realizing that I wanted to develop in a style which integrates both of these paradigms.

In particular, I want my data structures to be mostly immutable and the logic of my program to be pure and without side effects, and to keep my object graphs immutable. At the same time, I'd like to structure my data via standard classes and have the possibility to copy parts of my object graphs and mutate these copies in order to produce new structures while maximizing data sharing.

For such a programming style, the following assumptions should hold true:

* Data should be immutable and structure sharing should be common wherever possible.
  * It should be possible to use imperative logic and operations wherever doing so does not mutate existing data.
* Data should be represented via primitive Common Lisp types and standard classes.
  * It should be possible to use cyclic references for programming convenience.
* It should be possible to consider a type mismatch an abnormal situation while comparing for equivalence.
  * It should be possible to use `:type` keyword arguments for class slots and have runtime assertions for type checks without risking undefined behavior or depending on implementation-defined behavior.
* Value semantics should be used to compare data for equivalence.
  * Two Lisp objects should be recognized as equivalent if their types and contents are equivalent.
  * It should be possible to extend the equivalence predicate with user code.
  * Cyclic data structures which are impossible to tell apart value-wise (e.g. `#1=(1 2 3 . #1#)` and `#2=(1 2 3 1 2 3 . #2#)`) should be recognized as equivalent.
* For convenice and type safety, there should be a way to ensure, on the MOP level, that a slot in an instance is:
  * meant to be always bound,
  * meant to always contain a value of a particular type.

In order to be able to work with these assumptions, it is important to define [operations for copying and equivalence](http://www.nhplace.com/kent/PS/EQUAL.html), as well as define their effects on data structures which we want to operate on. This repository contains a collection of utilities implementing this programming style.

## Manual

There are several sources of authority for the code in this repository:

* the [`EQV` manual](doc/EQV.md),
* the copier-modifier manual (TODO),
* the [classes manual](doc/CLASSES.md),
* the [set manual](doc/SET.md),
* the [test suite](t/) containing more examples and edge cases.

## Testing

`(asdf:test-system :value-semantics-utils)`

## License

MIT.

## Tested on

`EQV` contains only portable code should work on every Common Lisp implementation. The classes might not, because CLOS and MOP are hard.

The classes were tested on SBCL 2.2.9 with some custom fixups. Nowhere else, yet. Expect breakage because of high doses of MOP wizardry, even though this library uses `closer-mop`.

On SBCL, we need to wait for https://bugs.launchpad.net/sbcl/+bug/1956621 to get fixed ~~and for the [patch](https://sourceforge.net/p/sbcl/mailman/sbcl-devel/thread/6ae094ba-eeea-6bfe-b43d-970d97040830%40disroot.org/) that stabilizes behavior for failed `U-I-F-{R,D}-C` to get merged~~. Sigh. MOP is hard. MOP interactions with everything else are even harder.

## Lemme in, LEMME IIIINNNNNNN

If you don't want to wait for SBCL to catch up, evaluate [this](sbcl-fixup.lisp) in order to get the tests to pass. Trust me, I'm an engineer.
