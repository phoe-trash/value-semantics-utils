(in-package #:value-semantics-utils)

;;; EQV
(setf
 (documentation 'eqv 'function)
 "A programmable equivalence predicate with cycle detection.

See GENERIC-EQV"
 (documentation 'generic-eqv 'function)
 "A generic function implementing comparison behavior for EQV.

See EQV"
 (documentation '*eqv-default-method-behavior* 'variable)
 "A global dynamic variable which drives the behavior when the default
method for GENERIC-EQV is called. If NIL, then NIL is returned; if any of
SIGNAL, WARN, or ERROR, then the respective CL function is used to signal a
condition of type EQV-DEFAULT-METHOD-CALLED.

See EQV-DEFAULT-METHOD-CALLED."
 (documentation 'eqv-default-method-called 'type)
 "The condition type signaled whenever the EQV default method is called.

See *EQV-DEFAULT-METHOD-BEHAVIOR*
See EQV-DEFAULT-METHOD-CALLED-X
See EQV-DEFAULT-METHOD-CALLED-Y"
 (documentation 'eqv-default-method-called-x 'function)
 "The first argument that the EQV default method was called with.

See EQV-DEFAULT-METHOD-CALLED"
 (documentation 'eqv-default-method-called-y 'function)
 "The second argument that the EQV default method was called with.

See EQV-DEFAULT-METHOD-CALLED")

;;; CLASS-WITH-VALUE-SEMANTICS
(setf
 (documentation 'class-with-value-semantics 'type)
 "A metaclass for classes meant to be comparable via EQV. Whenever this class is
instantiated, a new method on GENERIC-EQV is added, specialized on two
instances of this class; the instances are compared slotwise via
GENERIC-EQV.

See EQV
See GENERIC-EQV")

;;; ALWAYS-BOUND-CLASS
(setf
 (documentation 'always-bound-class 'type)
 "A metaclass for classes whose slots should never be unbound, be it via
creating an instance without providing an initform or initarg or by
SLOT-MAKUNBOUND. In all situations in which a slot might become unbound, a
condition of type UNBOUND-SLOT is signaled instead.")

;;; TYPECHECKED-CLASS
(setf
 (documentation 'typechecked-class 'type)
 "A metaclass for classes whose slots should never contain a value not of the
provided type, be it via creating an instance with an invalid initform or by
SETF SLOT-VALUE. In all situations in which a slot might come to contain a value
of invalid type, a condition of type TYPE-ERROR is signaled instead.

See ALWAYS-BOUND-CLASS"
 (documentation 'typechecked-slot-definition 'type)
 "A typechecked slot definition metaclass.

See TYPECHECKED-EFFECTIVE-SLOT-DEFINITION"
 (documentation 'typechecked-effective-slot-definition 'type)
 "A typechecked effective slot definition metaclass, containing a typecheck
function used to validate values stored into the slot.

See TYPECHECKED-SLOT-DEFINITION
See SLOT-DEFINITION-TYPECHECK-FUNCTION"
 (documentation 'slot-definition-typecheck-function 'function)
 "An accessor function for accessing the typecheck function of a typechecked
effective slot definition.

See TYPECHECKED-EFFECTIVE-SLOT-DEFINITION")

;;; TYPECHECKED-CLASS-WITH-VALUE-SEMANTICS
(setf
 (documentation 'typechecked-class-with-value-semantics 'type)
 "A metaclass for typechecked classes meant to be comparable via EQV.

See EQV
See TYPECHECKED-CLASS
See CLASS-WITH-VALUE-SEMANTICS")
