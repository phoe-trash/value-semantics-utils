(in-package #:value-semantics-utils/test)

(defclass test-typechecked-class ()
  ((slot-1 :initarg :slot-1
           :initform 42
           :type integer)
   (slot-2 :initarg :slot-2
           :type null)
   (slot-3 :initarg :slot-3
           :type keyword))
  (:metaclass vs:typechecked-class))

(define-test typechecked-class :parent value-semantics-utils
  (flet ((make (&rest args)
           (apply #'make-instance 'test-typechecked-class args)))
    (fail (make) unbound-slot)
    (fail (make :slot-2 :foo) type-error)
    (fail (make :slot-2 42) type-error)
    (fail (make :slot-2 nil) unbound-slot)
    (fail (make :slot-2 nil :slot-3 'foo) type-error)
    (let ((instance (make :slot-2 nil :slot-3 :foo)))
      (fail (slot-makunbound instance 'slot-1) unbound-slot)
      (fail (slot-makunbound instance 'slot-2) unbound-slot)
      (fail (slot-makunbound instance 'slot-3) unbound-slot)
      (let* ((class (class-of instance)))
        (dolist (esd (m:class-slots class))
          (let* ((fn (vs:slot-definition-typecheck-function esd)))
            (fail (funcall fn "foo") type-error)))))))

(defclass test-typechecked-class-inheritance-1 ()
  ((slot :initarg :slot :type (integer 0 100)))
  (:metaclass vs:typechecked-class))

(defclass test-typechecked-class-inheritance-2
    (test-typechecked-class-inheritance-1)
  ((slot :initarg :slot :type (integer 50 100)))
  (:metaclass vs:typechecked-class))

(define-test typechecked-class-inheritance :parent value-semantics-utils
  (flet ((make-1 (&rest args)
           (apply #'make-instance 'test-typechecked-class-inheritance-1 args))
         (make-2 (&rest args)
           (apply #'make-instance 'test-typechecked-class-inheritance-2 args)))
    (make-1 :slot 20)
    (fail (make-2 :slot 20) type-error)))
