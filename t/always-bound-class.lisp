(in-package #:value-semantics-utils/test)

(defclass test-always-bound-class ()
  ((slot :initarg :slot))
  (:metaclass vs:always-bound-class))

(define-test always-bound-class :parent value-semantics-utils
  (fail (make-instance 'test-always-bound-class) 'unbound-slot)
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (use-value 42 c))))
      (setf instance (make-instance 'test-always-bound-class))
      (is = 42 (slot-value instance 'slot))))
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (store-value 42 c))))
      (setf instance (make-instance 'test-always-bound-class)))
    (is = 42 (slot-value instance 'slot))
    (setf (slot-value instance 'slot) 24)
    (is = 24 (slot-value instance 'slot))
    (handler-bind ((unbound-slot (lambda (c) (store-value 0 c))))
      (slot-makunbound instance 'slot))
    (#-sbcl progn
     #+sbcl skip #+sbcl "https://bugs.launchpad.net/sbcl/+bug/1956621"
     (is = 0 (print (slot-value instance 'slot))))))
