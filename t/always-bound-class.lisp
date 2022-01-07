(in-package #:value-semantics-utils/test)

(defclass test-always-bound-class ()
  ((slot :initarg :slot))
  (:metaclass vs:always-bound-class))

(define-test always-bound-class :parent value-semantics-utils)

(define-test always-bound-class-fail :parent always-bound-class
  (fail (make-instance 'test-always-bound-class) 'unbound-slot))

(define-test always-bound-class-use-value :parent always-bound-class
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (use-value 42 c))))
      (setf instance (make-instance 'test-always-bound-class))
      (is = 42 (slot-value instance 'slot)))))

(define-test always-bound-class-store-value :parent always-bound-class
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (store-value 42 c))))
      (setf instance (make-instance 'test-always-bound-class))
      (is = 42 (slot-value instance 'slot)))))

(define-test always-bound-class-setf-slot-value :parent always-bound-class
  (let ((instance (make-instance 'test-always-bound-class :slot 42)))
    (is = 42 (slot-value instance 'slot))
    (setf (slot-value instance 'slot) 24)
    (is = 24 (slot-value instance 'slot))))

(define-test always-bound-class-slot-makunbound :parent always-bound-class
  (let ((instance (make-instance 'test-always-bound-class :slot 42)))
    (handler-bind ((unbound-slot (lambda (c) (store-value 0 c))))
      (slot-makunbound instance 'slot))
    (is = 0 (slot-value instance 'slot))))

(define-test always-bound-class-reinitialize-instance :parent always-bound-class
  (let ((instance (make-instance 'test-always-bound-class :slot 42)))
    (reinitialize-instance instance :slot 24)
    (is = 24 (slot-value instance 'slot))))
