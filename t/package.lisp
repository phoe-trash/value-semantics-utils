(uiop:define-package #:value-semantics-utils/test
  (:use #:cl #:parachute)
  (:local-nicknames (#:a #:alexandria)
                    (#:m #:closer-mop)
                    (#:u #:moptilities)
                    (#:vs #:value-semantics-utils))
  (:export #:value-semantics-utils))

(in-package #:value-semantics-utils/test)

(define-test value-semantics-utils)
