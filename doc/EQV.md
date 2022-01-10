# `EQV` - the programmable equivalence predicate

## API

### **Function `EQV`**

```lisp
(eqv x y) → boolean
```

An equivalence predicate that acts similar to `EQUAL` or `EQUALP`. It is user-extensible via `GENERIC-EQV`, can be configured to not hang on cycles, and to possibly signal a `EQV-DEFAULT-METHOD-CALLED` condition in case of fallthrough to the default method.

### **Generic Function `GENERIC-EQV`**

```lisp
(generic-eqv x y) → boolean
```

A means of programming `EQV`. Not meant to be called directly, programmers can write methods for it though (see "Extending").

Methods are defined for `X` and `Y` both being of the following class:
* `FUNCTION` - compares via `EQ`;
* `SYMBOL` - compares interned symbols via `EQ` and uninterned symbols via `STRING=` of their names;
* `PACKAGE` - compares via `EQ`;
* `STREAM` - compares via `EQ`;
* `NUMBER` - compares via `=`;
* `STRING` - compares via `STRING=`;
* `PATHNAME` - compares via `EQUAL`;
* `CONS` - compares the `CAR` and `CDR` recursively via `GENERIC-EQV`;
* `ARRAY` - compares array dimensions via `EQUAL`, then compares elements recursively via `GENERIC-EQV`;
* `HASH-TABLE` - compares hash table counts via `=`, then compares hash table test via `EQ`, then compares keys and values recursively via `GENERIC-EQV`;
* `OBJECT-WITH-VALUE-SEMANTICS` (see [the classes manual](CLASSES.md)) - compares the objects' classes via `EQ`, then recursively compares slot values via `GENERIC-EQV`;
* `T` - maybe signals a `EQV-DEFAULT-METHOD-CALLED`, then fails the comparison.

For conses, arrays and hash-tables, `EQV` are defined to work similarly to `EQUALP`, except it uses `GENERIC-EQV` for recursively comparing elements.

### **Variable `*EQV-RESOLVE-CYCLES-P*`** 

A dynamic variable controlling whether `EQV` will check object identity (via `EQ`) to detect cycles. Defaults to true. Rebind it to false for saving some memory if you are **sure** that the data you are comparing contains no cycles.

```lisp
VALUE-SEMANTICS-UTILS> (eqv '#1=(1 2 3 . #1#) '#2=(1 2 3 . #2#))
T

VALUE-SEMANTICS-UTILS> (let ((*eqv-resolve-cycles-p* nil))
                         (eqv '#1=(1 2 3 . #1#) '#2=(1 2 3 . #2#)))
;;; loops until the heap goes boom
```

Note that cycle detection is capable of detecting cycles which are equivalent value-wise, but whose underlying storage has different length.

```lisp
VALUE-SEMANTICS-UTILS> (eqv '#1=(1 2 3 1 2 3 . #1#) '#2=(1 2 3 . #2#))
T

VALUE-SEMANTICS-UTILS> (eqv '#1=(1 2 3 . #1#) '#2=(1 2 3 1 2 3 . #2#))
T

VALUE-SEMANTICS-UTILS> (eqv '#1=(1 1 1 1 1 1 1 1 1 1 1 1 1 1 . #1#) '#2=(1 . #2#))
T
```

### **Condition Type `EQV-DEFAULT-METHOD-CALLED`**

A condition optionally signaled (see `*EQV-DEFAULT-METHOD-BEHAVIOR*`) when the default method on `GENERIC-EQV` is called.

### **Reader Function `EQV-DEFAULT-METHOD-CALLED-ARGS`**

A reader function for the arguments with which the default method on `GENERIC-EQV` was called.

### **Variable `*EQV-DEFAULT-METHOD-BEHAVIOR*`**

A dynamic variable controlling the signaling behavior of the default method on `GENERIC-EQV`. Allowed values are `NIL`, `SIGNAL`, `WARN`, and `ERROR` or any function which mimics the function signature of `SIGNAL`. Defaults to the symbol `WARN`.

```lisp
VALUE-SEMANTICS-UTILS> (let ((*eqv-default-method-behavior* nil))
                         (eqv 42 "42"))
NIL

VALUE-SEMANTICS-UTILS> (let ((*eqv-default-method-behavior* 'signal))
                         (flet ((note (c) (format t ";; ~A" c)))
                           (handler-bind ((eqv-default-method-called #'note))
                             (eqv 42 "42"))))
;; EQV default method called with (42 "42"); possible type error?
NIL

VALUE-SEMANTICS-UTILS> (let ((*eqv-default-method-behavior* 'warn)) ; the default
                         (eqv 42 "42"))
;;; WARNING: EQV default method called with (42 "42"); possible type error?
NIL

VALUE-SEMANTICS-UTILS> (let ((*eqv-default-method-behavior* 'error))
                         (eqv 42 "42"))
;;; Error: EQV default method called with (42 "42"); possible type error?
;;;   [Condition of type EQV-DEFAULT-METHOD-CALLED]

```

## Extending

`EQV` calls the generic function `GENERIC-EQV` in a continuation-passing style in order to avoid causing stack overflows on deeply nested structures or during cycle detection. This requires a particular style of writing methods on `GENERIC-EQV`.

A single call to `GENERIC-EQV` is meant to perform a single comparison and return four values. The first one is the result of that comparison; `EQV` immediately returns `NIL` if that value is false at any time. The second, third, and fourth values are meaningful if a data structure is nested; the second and third value are the next objects to be compared via `GENERIC-EQV` (or `NIL` if there are no objects to compare) and the fourth value is a continuation function (or `NIL` if there is nothing left to compare) that should, similarly, return four values for the next pair of objects stored inside that recursive data structure.

Of course, if a particular method can ensure that it can compare its arguments without deep recursion, it is free to compute its result directly and return a generalized boolean as its first valie and return `NIL` as its secondary, tertiary, and quaternary values to indicate that. (See e.g. the methods defined for `SYMBOL` or `STRING`.)

Let's consider a small example: let's manually check if `'(1 (2 3))` is equivalent to `'(1 (2 3))` using only `GENERIC-EQV`.

```lisp
VALUE-SEMANTICS-UTILS> (generic-eqv '(1 (2 3)) '(1 (2 3)))
T
1
1
#<FUNCTION (LABELS CDR-CONTINUATION :IN GENERIC-EQV) {100BF70FDB}>
```

In this call, we get four values: the first value is true, and that means that we should continue comparing. The second and third values are values to be recursively compared via `GENERIC-EQV`, in this case, the `CAR`s of the cons cells; it calls `=` for numbers, so we'll skip that. The fourth value is a continuation function that should be called in order to give us the next set of values, in this case, the `CDR`s of these cons cells.

```lisp
VALUE-SEMANTICS-UTILS> (funcall (fourth /))
T
((2 3))
((2 3))
NIL
```

We see that the fourth return value is `NIL`, which means that the data structure has no more values that we should compare. That's understandable - a cons cell can only refer to two objects. We are free to compare the second and third value using `GENERIC-EQV`:

```lisp
VALUE-SEMANTICS-UTILS> (generic-eqv (second /) (third /))
T
(2 3)
(2 3)
#<FUNCTION (LABELS CDR-CONTINUATION :IN GENERIC-EQV) {100BF821EB}>
```

And this goes on either until the first returned value is `NIL`, which immediately fails the comparison, or until the first value is true and the three remaining ones are `NIL`, which means that there is nothing left to compare.

`EQV` also correctly handles a situation in which it needs to defer calling a continuation for later in order to compare the returned values first for preserving traversal order. It's done by allocating a closure on the heap, in which the values are compared by `GENERIC-EQV` and in which the continuation object will eventually be returned for processing.

Why are the second and third values returned if, in theory, all we need is a boolean value to figure out if we need to continue the comparison and a continuation object to continue comparing? The answer is that `EQV` is capable of performing cycle detection, for which it needs to access the objects themselves in order to check their identity for cycle detection.
