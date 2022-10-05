(uiop:define-package #:value-semantics-utils
  (:use #:c2cl)
  (:shadow #:set #:set-difference #:set-exclusive-or)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:moptilities)
                    (#:w #:with-branching))
  (:export
   ;; EQV
   #:eqv
   #:generic-eqv
   #:*eqv-default-method-behavior*
   #:eqv-default-method-called
   #:eqv-default-method-called-args
   ;; CLASS-WITH-VALUE-SEMANTICS
   #:object-with-value-semantics
   #:copy
   #:class-with-value-semantics
   ;; ALWAYS-BOUND-CLASS
   #:always-bound-object
   #:always-bound-class
   ;; TYPECHECKED-CLASS
   #:typechecked-object
   #:typechecked-class
   ;; TYPECHECKED-CLASS-WITH-VALUE-SEMANTICS
   #:typechecked-class-with-value-semantics
   ;; SET
   #:set #:set-test #:set-contents
   #:set-insert #:set-remove #:set-find
   #:set-difference #:set-union #:set-intersection #:set-exclusive-or))
