# VALUE-SEMANTICS-UTILS Classes

## Value semantics

### **Class `CLASS-WITH-VALUE-SEMANTICS`**

A metaclass whose metainstances are automatically comparable slotwise via `EQV`.

### **Class `OBJECT-WITH-VALUE-SEMANTICS`**

An automatic subclass of all instances of every `CLASS-WITH-VALUE-SEMANTICS`.

```lisp
CL-USER> (defclass foo ()
           ((slot :initarg :slot))
           (:metaclass class-with-value-semantics))
#<CLASS-WITH-VALUE-SEMANTICS CL-USER::FOO>

CL-USER> (eqv (make-instance 'foo)
              (make-instance 'foo))
T

CL-USER> (eqv (make-instance 'foo :slot 42)
              (make-instance 'foo :slot 42))
T

CL-USER> (eqv (make-instance 'foo :slot 42)
              (make-instance 'foo))
NIL

CL-USER> (eqv (make-instance 'foo :slot 42)
              (make-instance 'foo :slot "42"))
;;; WARNING: EQV default method called with (42 "42"); possible type error?
NIL

CL-USER> (eqv (make-instance 'foo :slot (make-instance 'foo))
              (make-instance 'foo :slot (make-instance 'foo)))
T

CL-USER> (defclass bar ()
           ((slot :initarg :slot))
           (:metaclass class-with-value-semantics))
#<CLASS-WITH-VALUE-SEMANTICS CL-USER::BAR>

CL-USER> (eqv (make-instance 'foo :slot 42)
              (make-instance 'bar :slot 42))
NIL
```

## Always-bound

### **Class `ALWAYS-BOUND-CLASS`**

A metaclass whose instances are meant to never have their slots unbound.

### **Class `ALWAYS-BOUND-OBJECT`**

An automatic subclass of all instances of every `ALWAYS-BOUND-CLASS`.

```lisp
CL-USER> (defclass baz ()
           ((slot :initarg :slot))
           (:metaclass always-bound-class))
#<ALWAYS-BOUND-CLASS CL-USER::BAZ>

CL-USER> (make-instance 'baz)
;;; Error: The slot CL-USER::SLOT is unbound in the object #<BAZ {10026C0E93}>.
;;;    [Condition of type UNBOUND-SLOT]

CL-USER> (make-instance 'baz :slot 42)
#<BAZ {100290BE73}>

CL-USER> (slot-makunbound * 'slot)
;;; Error: The slot CL-USER::SLOT is unbound in the object #<BAZ {100290BE73}>.
;;;   [Condition of type UNBOUND-SLOT]
;;;
;;; Aborting to toplevel.

CL-USER> (slot-value * 'slot)
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

## Typechecking

### **Class `TYPECHECKED-CLASS`**

A metaclass with mandatory runtime typechecking for slot values. Subclasses `ALWAYS-BOUND-CLASS`.

### **Class `TYPECHECKED-OBJECT`**

An automatic subclass of all instances of every `TYPECHECKED-CLASS`.

```lisp
CL-USER> (defclass quux ()
           ((slot :initarg :slot :type integer))
           (:metaclass typechecked-class))
#<TYPECHECKED-CLASS CL-USER::QUUX>

CL-USER> (make-instance 'quux)
;;; Error: The slot CL-USER::SLOT is unbound in the object #<QUUX {1005907C23}>.
;;;   [Condition of type UNBOUND-SLOT]

CL-USER> (make-instance 'quux :slot "42")
;;; Error: The value "42" is not of type INTEGER.
;;;   [Condition of type TYPE-ERROR]

CL-USER> (make-instance 'quux :slot 42)
#<QUUX {1005C9A643}>

CL-USER> (setf (slot-value * 'slot) "42")
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

## The final form™

### **Class `TYPECHECKED-CLASS-WITH-VALUE-SEMANTICS`**

A metaclass composing the above three metaclasses.
