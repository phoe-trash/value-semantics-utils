(uiop:define-package #:value-semantics-utils
  (:use #:c2cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:moptilities))
  (:export
   ;; EQV
   #:eqv
   #:eqv-using-class
   #:*eqv-resolve-cycles-p*
   #:*eqv-default-method-behavior*
   #:eqv-default-method-called
   #:eqv-default-method-called-args
   ;; CLASS-WITH-VALUE-SEMANTICS
   #:object-with-value-semantics
   #:class-with-value-semantics
   ;; ALWAYS-BOUND-CLASS
   #:always-bound-object
   #:always-bound-class
   ;; TYPECHECKED-CLASS
   #:typechecked-object
   #:typechecked-class
   ;; TYPECHECKED-CLASS-WITH-VALUE-SEMANTICS
   #:typechecked-class-with-value-semantics))
