# VALUE-SEMANTICS-UTILS

Utilities for using CLOS for mostly-functional programming.

## Why?

There are two worlds in programming: an imperative one, full of objects and comparisons by identity, and a functional one, full of referential transparency and comparisons by value. Common Lisp is good at both, and I ended up realizing that I wanted to develop a program which integrates both of these paradigms.

In particular, I want my data structures to be mostly immutable and the logic of my program to be pure and without side effects, but I'd like to structure my data via standard classes and have the possibility to mutate objects wherever it cannot be observed in the higher-level logic.

For such a programming style, the following assumptions should hold true:

* Data should be immutable and structure sharing should be common wherever possible.
  * It should be possible to use imperative logic wherever doing so does not mutate existing data.
* Data should be represented via primitive Common Lisp types and standard classes.
  * It should be possible to use cyclic references for programming convenience.
* It should be possible to consider a type mismatch an abnormal situation while comparing for equivalence.
  * It should be possible to use `:type` arguments for class slots and have runtime assertions for type checks without risking undefined behavior or depending on implementation-defined behavior.
* Value semantics should be used to compare data for ~~equality~~equivalence. In particular, two classes should be equivalent if their types and contents are equivalent.
  * The only exception to using pure value semantics should be cycle detection in data structures, for which there seems to be no solution better than identity comparison.
    * (Thankfully, there are no generators or infinite lists in Lisp.)
* There should be a way to ensure, on the MOP level, that a slot in an instance is...
  * ...meant to be always bound,
  * ...meant to always contain a value of a particular type.

This repository contains a series of utilities meant to facilitate this style of programming.

## Manual

Take a look at [the tests](t/) for more examples and edge cases.

### Equivalence

#### **Function `EQV`**

```lisp
(eqv x y) → boolean
```

An equivalence predicate that acts similar to `EQUAL` or `EQUALP`. It is user-extensible via `EQV-USING-CLASS`, can be configured to not hang on cycles, and to possibly signal a `EQV-DEFAULT-METHOD-CALLED` condition in case of fallthrough to the default method.

#### **Generic Function `EQV-USING-CLASS`**

```lisp
(eqv-using-class x y compare-fn fail-rn) → boolean
```

A means of programming `EQV`. Not meant to be called directly; programmers can write methods for it though - see "Extending".

Methods are defined for `X` and `Y` of the following arguments:
* `FUNCTION` - compares via `EQ`;
* `SYMBOL` - compares via `EQ`;
* `PACKAGE` - compares via `EQ`;
* `STREAM` - compares via `EQ`;
* `NUMBER` - compares via `=`;
* `STRING` - compares via `STRING=`;
* `PATHNAME` - compares via `EQUAL`;
* `CONS` - compares the `CAR` and `CDR` recursively via `EQV-USING-CLASS`;
* `ARRAY` - compares array dimensions via `EQUAL`, then compares elements recursively via `EQV-USING-CLASS`;
* `HASH-TABLE` - compares hash table counts via `=`, then compares hash table test via `EQ`, then compares keys and values recursively via `EQV-USING-CLASS`;
* `OBJECT-WITH-VALUE-SEMANTICS` (see below) - compares the objects' classes via `EQ`, then recursively compares slot values via `EQV-USING-CLASS`;
* `T` - maybe signals a `EQV-DEFAULT-METHOD-CALLED`, then fails the comparison.

For conses, arrays and hash-tables, `EQV` are defined to work similarly to `EQUALP`, except it uses `EQV-USING-CLASS` for recursively comparing elements.

#### **Variable `*EQV-RESOLVE-CYCLES-P*`** 

A dynamic variable controlling whether `EQV` will check object identity (via `EQ`) to detect cycles. Defaults to true. Rebind it to false for saving some memory if you are **sure** that the data you are comparing contains no cycles.

```lisp
VALUE-SEMANTICS-UTILS> (eqv '#1=(1 2 3 . #1#) '#2=(1 2 3 . #2#))
T

VALUE-SEMANTICS-UTILS> (let ((*eqv-resolve-cycles-p* nil))
                         (eqv '#1=(1 2 3 . #1#) '#2=(1 2 3 . #2#)))
;;; the stack goes boom
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

#### **Condition Type `EQV-DEFAULT-METHOD-CALLED`**

A condition optionally signaled (see `*EQV-DEFAULT-METHOD-BEHAVIOR*`) when the default method on `EQV-USING-CLASS` is called.

#### **Reader Function `EQV-DEFAULT-METHOD-CALLED-ARGS`**

A reader function for the arguments with which the default method on `EQV-USING-CLASS` was called.

#### **Variable `*EQV-DEFAULT-METHOD-BEHAVIOR*`**

A dynamic variable controlling the signaling behavior of the default method on `EQV-USING-CLASS`. Allowed values are `NIL`, `SIGNAL`, `WARN`, and `ERROR` or any function which mimics the function signature of `SIGNAL`. Defaults to the symbol `WARN`.

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

### Value semantics

#### **Class `CLASS-WITH-VALUE-SEMANTICS`**

A metaclass whose metainstances are automatically comparable slotwise via `EQV`.

#### **Class `OBJECT-WITH-VALUE-SEMANTICS`**

An automatic subclass of all instances of every `CLASS-WITH-VALUE-SEMANTICS`.

```lisp
VALUE-SEMANTICS-UTILS> (defclass foo () 
                         ((slot :initarg :slot))
                         (:metaclass class-with-value-semantics))
#<CLASS-WITH-VALUE-SEMANTICS VALUE-SEMANTICS-UTILS::FOO>

VALUE-SEMANTICS-UTILS> (eqv (make-instance 'foo)
                            (make-instance 'foo))
T

VALUE-SEMANTICS-UTILS> (eqv (make-instance 'foo :slot 42)
                            (make-instance 'foo :slot 42))
T

VALUE-SEMANTICS-UTILS> (eqv (make-instance 'foo :slot 42)
                            (make-instance 'foo))
NIL

VALUE-SEMANTICS-UTILS> (eqv (make-instance 'foo :slot 42)
                            (make-instance 'foo :slot "42"))
;;; WARNING: EQV default method called with (42 "42"); possible type error?
NIL

VALUE-SEMANTICS-UTILS> (eqv (make-instance 'foo :slot (make-instance 'foo))
                            (make-instance 'foo :slot (make-instance 'foo)))
T

VALUE-SEMANTICS-UTILS> (defclass bar () 
                         ((slot :initarg :slot))
                         (:metaclass class-with-value-semantics))
#<CLASS-WITH-VALUE-SEMANTICS VALUE-SEMANTICS-UTILS::BAR>

VALUE-SEMANTICS-UTILS> (eqv (make-instance 'foo :slot 42)
                            (make-instance 'bar :slot 42))
NIL
```

### Always-bound

#### **Class `ALWAYS-BOUND-CLASS`**

A metaclass whose instances are meant to never have their slots unbound.

#### **Class `ALWAYS-BOUND-OBJECT`**

An automatic subclass of all instances of every `ALWAYS-BOUND-CLASS`.

```lisp
VALUE-SEMANTICS-UTILS> (defclass baz () 
                         ((slot :initarg :slot))
                         (:metaclass always-bound-class))
#<ALWAYS-BOUND-CLASS VALUE-SEMANTICS-UTILS::BAZ>

VALUE-SEMANTICS-UTILS> (make-instance 'baz)
;;; Error: The slot VALUE-SEMANTICS-UTILS::SLOT is unbound in the object #<BAZ {10026C0E93}>.
;;;    [Condition of type UNBOUND-SLOT]

VALUE-SEMANTICS-UTILS> (make-instance 'baz :slot 42)
#<BAZ {100290BE73}>

VALUE-SEMANTICS-UTILS> (slot-makunbound * 'slot)
;;; Error: The slot VALUE-SEMANTICS-UTILS::SLOT is unbound in the object #<BAZ {100290BE73}>.
;;;   [Condition of type UNBOUND-SLOT]
;;;
;;; Aborting to toplevel.

VALUE-SEMANTICS-UTILS> (slot-value * 'slot)
42
```

For an always-bound object, the following should hold true:

* Attempting to initialize an instance with an unbound slot should signal an `unbound-slot` error.
* Reinitializing an instance should, by definition, never introduce unbound slots in a class, but *just to be absolutely sure*, attempting to reinitialize an instance with an unbound slot should signal an `unbound-slot` error.
* Redefining an always-bound class in a way that would cause an instance to gain an unbound slot should cause *any* accesses to that instance to repeatedly signal an `unbound-slot` error until the class is redefined in a way which no longer results in the instance gaining an unbound slot.
* Attempting to change the class of an instance to an always-bound class in a way that would cause an instance to gain an unbound slot should signal an `unbound-slot` error.
* Calling `slot-makunbound` on an instance should - guess what - signal an `unbound-slot` error.

All errors signaled from the above situations should be correctable via both `use-value` and `store-value` restarts available; both of these restarts should work the same, as the provided value will be stored in the slot, after which execution will continue.

TODO: change this to only offer a `store-value` restart.

### Typechecking

#### **Class `TYPECHECKED-CLASS`**

A metaclass with mandatory runtime typechecking for slot values. Subclasses `ALWAYS-BOUND-CLASS`.

#### **Class `TYPECHECKED-OBJECT`**

An automatic subclass of all instances of every `TYPECHECKED-CLASS`.

```lisp
VALUE-SEMANTICS-UTILS> (defclass quux ()
                         ((slot :initarg :slot :type integer))
                         (:metaclass typechecked-class))
#<TYPECHECKED-CLASS VALUE-SEMANTICS-UTILS::QUUX>

VALUE-SEMANTICS-UTILS> (make-instance 'quux)
;;; Error: The slot VALUE-SEMANTICS-UTILS::SLOT is unbound in the object #<QUUX {1005907C23}>.
;;;   [Condition of type UNBOUND-SLOT]

VALUE-SEMANTICS-UTILS> (make-instance 'quux :slot "42")
;;; Error: The value "42" is not of type INTEGER.
;;;   [Condition of type TYPE-ERROR]

VALUE-SEMANTICS-UTILS> (make-instance 'quux :slot 42)
#<QUUX {1005C9A643}>

VALUE-SEMANTICS-UTILS> (setf (slot-value * 'slot) "42")
;;; Error: The value "42" is not of type INTEGER.
;;;   [Condition of type TYPE-ERROR]
```

For a typechecked object, the following should hold true:

* Attempting to initialize an instance with a mistyped slot should signal a `type-error` error.
* Attempting to reinitialize an instance with a mistyped slot should signal a `type-error` error.
* Redefining an typeched class in a way that would cause an instance to gain a mistyped slot should cause *any* accesses to that instance to repeatedly signal an`type-error` error until the class is redefined in a way which no longer results in the instance gaining a mistyped slot.
* Attempting to change the class of an instance to a typechecked class in a way that would cause an instance to gain a mistyped slot should signal a `type-error` error.
* Calling `setf slot-value` on an instance with a mistyped new value should - guess what - signal a `type-error` error.

All errors signaled from the above situations should be correctable via both `use-value` and `store-value` restarts available; both of these restarts should work the same, as the provided value will be stored in the slot, after which execution will continue.

TODO: change this to only offer a `store-value` restart.

### The final form™

#### **Class `TYPECHECKED-CLASS-WITH-VALUE-SEMANTICS`**

A metaclass composing the above three metaclasses.

## Extending

`EQV` uses the generic function `EQV-USING-CLASS` in a continuation-passing sty-- Oh who am I kidding, I have no idea how to nicely explain how to extend `EQV-USING-CLASS` with the current CPSesque architecture. Let me explain it in a non-nice way.

`EQV-USING-CLASS` accepts four arguments - the first two are the elements to be compared, and the first two are functions that are meant to be called. The value returned from `EQV-USING-CLASS` is ignored; a comparison failure must be performed via funcalling the `FAIL-FN` argument.

When it comes to comparing primitive types such as strings or symbols, it is enough to funcall the `FAIL-FN` if the two objects are not equivalent. For recursive data structures, though, the protocol is slightly more involved.

If a comparison requires making any recursive comparisons, `EQV` expects that `COMPARE-FN` will be called in `EQV-USING-CLASS`. The two mandatory arguments to that function must be values to be compared the next time. If there is more than one recursive comparison required, the third (optional) argument to that function should be a zero-arg continuation function that will call `COMPARE-FN` again for the next pair of values. Take a look at the implementations for conses, arrays, and hash-tables to get a feel for how you should proceed.

As long as this repository is in `phoe-trash`, this protocol might change. And let's hope it does - the current one absolutely sucks.

## License

MIT.

## Tested on

SBCL 2.1.11 with some custom fixups. Nowhere else, yet. Expect breakage because of high doses of MOP wizardry, even though this library uses `closer-mop`.

On SBCL, we need to wait for https://bugs.launchpad.net/sbcl/+bug/1956621 to get fixed and for the [patch](https://sourceforge.net/p/sbcl/mailman/sbcl-devel/thread/6ae094ba-eeea-6bfe-b43d-970d97040830%40disroot.org/) that stabilizes behavior for failed `U-I-F-{R,D}-C` to get merged. Sigh. MOP is hard. MOP interactions with everything else are even harder.

If you don't want to wait for SBCL to catch up, evaluate the following in order to get the tests to pass.

Trust me, I'm an engineer.

```lisp

(in-package #:sb-pcl)

(defmacro wrapper-class (wrapper)
  `(classoid-pcl-class (wrapper-classoid ,wrapper)))

(sb-ext:without-package-locks
  (defun slot-makunbound (object slot-name)
    (let* ((class (class-of object))
           (slotd (find slot-name (class-slots class)
                        :key #'slot-definition-name))
           (actual-methods (compute-applicable-methods
                            #'slot-makunbound-using-class
                            (list class object slotd)))
           (actual-method (first actual-methods))
           (original-methods
             (list (find-method
                    #'slot-makunbound-using-class nil
                    (list (find-class 'std-class)
                          (find-class 'standard-object)
                          (find-class 'standard-effective-slot-definition)))
                   (find-method
                    #'slot-makunbound-using-class nil
                    (list (find-class 'condition-class)
                          (find-class 't)
                          (find-class 't)))
                   (find-method
                    #'slot-makunbound-using-class nil
                    (list (find-class 'structure-class)
                          (find-class 'structure-object)
                          (find-class 'structure-effective-slot-definition))))))
      (unless (member actual-method original-methods)
        (return-from slot-makunbound
          (slot-makunbound-using-class class object slotd))))
    (let* ((wrapper (valid-wrapper-of object))
           (cell (find-slot-cell wrapper slot-name))
           (location (car cell)))
      (cond ((fixnump location)
             (if (std-instance-p object)
                 (setf (standard-instance-access object location) +slot-unbound+)
                 (setf (funcallable-standard-instance-access object location)
                       +slot-unbound+)))
            ((not location)
             (if cell
                 (let ((class (wrapper-class wrapper)))
                   (slot-makunbound-using-class class object
                                                (find-slot-definition class slot-name)))
                 (slot-missing (wrapper-class wrapper) object slot-name
                               'slot-makunbound)))
            ((listp location) ; forcibly transform CONSP to LISTP
             (setf (cdr location) +slot-unbound+))
            (t
             (bug "Bogus slot-cell in SLOT-MAKUNBOUND: ~S" cell))))
    object))

(macrolet ((replace-wrapper-and-slots (thing layout slot-vector)
             `(if (functionp ,thing)
                  (setf (%fun-wrapper ,thing) ,layout
                        (fsc-instance-slots ,thing) ,slot-vector)
                  ;; TODO: use a double-wide CAS here if CPU supports it
                  (progn
                    (setf (%instance-wrapper ,thing) ,layout)
                    (%instance-set ,thing sb-vm:instance-data-start ,slot-vector)))))

(defun %obsolete-instance-trap (owrapper nwrapper instance)
  (cond
    ((layout-for-pcl-obj-p owrapper)
     (binding* ((class (wrapper-class nwrapper))
                (oslots (get-slots instance))
                (nwrapper (class-wrapper class))
                (nslots (make-array (wrapper-length nwrapper)
                                    :initial-element +slot-unbound+))
                (added ())
                (discarded ())
                (plist ())
                (safe (safe-p class))
                ((new-instance-slots nil new-custom-slots)
                 (classify-slotds (wrapper-slot-list nwrapper)))
                ((old-instance-slots old-class-slots old-custom-slots)
                 (classify-slotds (wrapper-slot-list owrapper)))
                (layout (mapcar (lambda (slotd)
                                  ;; Get the names only once.
                                  (cons (slot-definition-name slotd) slotd))
                                new-instance-slots)))
       ;; local  --> local     transfer value, check type
       ;; local  --> shared    discard value, discard slot
       ;; local  -->  --       discard slot
       ;; local  --> custom    XXX

       ;; shared --> local     transfer value, check type
       ;; shared --> shared    -- (cf SHARED-INITIALIZE :AFTER STD-CLASS)
       ;; shared -->  --       discard value
       ;; shared --> custom    XXX

       ;;  --    --> local     add slot
       ;;  --    --> shared    --
       ;;  --    --> custom    XXX
       (flet ((set-value (value cell)
                (%set-slot-value-checking-type
                 "updating obsolete instance"
                 nslots (cdr cell) value safe class class)
                ;; Prune from the list now that it's been dealt with.
                (setf layout (remove cell layout))))

         ;; Go through all the old local slots.
         (dolist (old old-instance-slots)
           (let* ((name (slot-definition-name old))
                  (cell (find-slot-cell owrapper name))
                  (location (car cell))
                  (value (cond
                           ((fixnump location)
                            (clos-slots-ref oslots location))
                           ((not location)
                            (let ((location (slot-info-location (cdr cell))))
                              (aver (integerp location))
                              (clos-slots-ref oslots (slot-info-location (cdr cell)))))
                           (t (bug "non-FIXNUM non-NULL location in cell: ~S" cell)))))
             (unless (unbound-marker-p value)
               (let ((new (assq name layout)))
                 (cond (new
                        (set-value value new))
                       (t
                        (push name discarded)
                        (setf (getf plist name) value)))))))

         ;; Go through all the old shared slots.
         (dolist (old old-class-slots)
           (binding* ((cell (slot-definition-location old))
                      (name (car cell))
                      (new (assq name layout) :exit-if-null))
             (set-value (cdr cell) new)))

         ;; Go through all custom slots to find added ones. CLHS
         ;; doesn't specify what to do about them, and neither does
         ;; AMOP. We do want them to get initialized, though, so we
         ;; list them in ADDED for the benefit of SHARED-INITIALIZE.
         (dolist (new new-custom-slots)
           (let* ((name (slot-definition-name new))
                  (old (find name old-custom-slots
                             :key #'slot-definition-name)))
             (unless old
               (push name added))))

         ;; Go through all the remaining new local slots to compute
         ;; the added slots.
         (dolist (cell layout)
           (push (car cell) added)))

       (replace-wrapper-and-slots instance nwrapper nslots)
       ;; The obsolete instance protocol does not specify what happens if
       ;; an error is signaled in U-I-F-R-C and there is a nonlocal exit
       ;; outside; it may result in a half-updated instance whose
       ;; structure is updated but whose added slots are not initialized.
       ;; (See CLHS 3.7.2.)
       ;; The approach taken here is to abort the update process, as defined
       ;; in CLHS 4.3.6, altogether, and restore the instance to its obsolete
       ;; state; this way the programmer can try to fix the U-I-F-R-C code
       ;; which signaled an error and try to access the instance again
       ;; in order to try and update it again.
       (sb-sys:nlx-protect (update-instance-for-redefined-class
                            instance added discarded plist)
         (replace-wrapper-and-slots instance owrapper oslots))

       nwrapper))
    (*in-obsolete-instance-trap* #.(find-layout 'structure-object))
    (t
     (let ((*in-obsolete-instance-trap* t))
       (error 'obsolete-structure :datum instance)))))

(defun %change-class (copy instance new-class initargs)
  (binding* ((new-wrapper (class-wrapper (ensure-class-finalized new-class)))
             (new-slots (make-array (wrapper-length new-wrapper)
                                    :initial-element +slot-unbound+))
             (old-wrapper (wrapper-of instance))
             (old-class (wrapper-class old-wrapper))
             (old-slots (get-slots instance))
             (safe (safe-p new-class))
             (new-wrapper-slots (wrapper-slot-list new-wrapper)))
    (replace-wrapper-and-slots copy new-wrapper new-slots)
    (flet ((initarg-for-slot-p (slot)
             (when initargs
               (dolist (slot-initarg (slot-definition-initargs slot))
                 (unless (unbound-marker-p
                          (getf initargs slot-initarg +slot-unbound+))
                   (return t)))))
           (set-value (value slotd)
             (%set-slot-value-checking-type
              'change-class new-slots slotd value safe
              old-class new-class)))

      ;; "The values of local slots specified by both the class CTO
      ;; and CFROM are retained. If such a local slot was unbound, it
      ;; remains unbound."
      (dolist (new new-wrapper-slots)
        (when (and (not (initarg-for-slot-p new))
                   (eq (slot-definition-allocation new) :instance))
          (binding* ((cell (find-slot-cell old-wrapper (slot-definition-name new))
                           :exit-if-null)
                     (location (car cell))
                     (value (cond
                              ((fixnump location)
                               (clos-slots-ref old-slots location))
                              ((not location)
                               (let ((info (cdr cell)))
                                 (case (slot-info-allocation info)
                                   (:instance
                                    (clos-slots-ref old-slots (slot-info-location info)))
                                   (:class (cdr (slot-info-location info))))))
                              (t
                               (cdr location)))))
            (set-value value new)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    ;; All uses of %CHANGE-CLASS are under the world lock, but that doesn't
    ;; preclude user code operating on the old slots + new layout or v.v.
    ;; Users need to synchronize their own access when changing class.
    (replace-wrapper-and-slots copy old-wrapper old-slots)
    (replace-wrapper-and-slots instance new-wrapper new-slots)

    ;; The CLHS does not specify what happens if an error is signaled in
    ;; U-I-F-D-C and there is a nonlocal exit outside; it may result in a
    ;; half-updated instance whose class is updated but whose added slots
    ;; are not initialized. (See CLHS 3.7.2.)
    ;; The approach taken here is to abort the change-class process, as
    ;; defined in CLHS 4.3.6, altogether, and restore the instance to its
    ;; previous state; this way the programmer can try to fix the U-I-F-D-C
    ;; code which signaled an error and try to CHANGE-CLASS the instance
    ;; again.
    (sb-sys:nlx-protect (apply #'update-instance-for-different-class
                               copy instance initargs)
      (replace-wrapper-and-slots instance old-wrapper old-slots))

    instance))
)

```
