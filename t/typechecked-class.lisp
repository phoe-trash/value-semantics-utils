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
      (fail (slot-makunbound instance 'slot-3) unbound-slot))))

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

(define-test typechecked-class-reinitialize-instance :parent typechecked-class
  (let ((instance (make-instance 'test-typechecked-class
                                 :slot-2 nil :slot-3 :foo)))
    (reinitialize-instance instance :slot-3 :bar)
    (is eq :bar (slot-value instance 'slot-3))))

(define-test typechecked-class-update-instance-for-redefined-class
  :parent typechecked-class
  (unwind-protect
       (progn
         (let (instance)
           (defclass test-typechecked-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1))
             (:metaclass vs:typechecked-class))
           (setf instance (make-instance 'test-typechecked-class-u-i-f-r-c
                                         :slot-1 42))
           (defclass test-typechecked-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1)
              (slot-2 :initarg :slot-2))
             (:metaclass vs:typechecked-class))
           (fail (slot-value instance 'slot-1) unbound-slot)
           (fail (slot-value instance 'slot-1) unbound-slot)
           (defclass test-typechecked-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1)
              (slot-2 :initarg :slot-2 :initform 24))
             (:metaclass vs:typechecked-class))
           (is = 42 (slot-value instance 'slot-1))
           (is = 24 (slot-value instance 'slot-2))
           (defclass test-typechecked-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1)
              (slot-2 :initarg :slot-2 :initform 24)
              (slot-3 :initarg :slot-3))
             (:metaclass vs:typechecked-class))
           (handler-bind ((unbound-slot (lambda (c) (store-value 0 c))))
             (slot-value instance 'slot-1))
           (is = 42 (slot-value instance 'slot-1))
           (is = 24 (slot-value instance 'slot-2))
           (is = 0 (slot-value instance 'slot-3))))
    (setf (find-class 'test-typechecked-class-u-i-f-r-c nil) nil)))

(defclass simple-class ()
  ((other-slot :initform 42)))

(define-test typechecked-class-update-instance-for-different-class
  :parent typechecked-class
  (let ((instance (make-instance 'simple-class)))
    (fail (change-class instance 'test-typechecked-class) unbound-slot)
    (true (typep instance 'simple-class))
    (is = 42 (slot-value instance 'other-slot))
    (change-class instance 'test-typechecked-class
                  :slot-2 nil :slot-3 :foo)
    (true (typep instance 'test-typechecked-class))
    (is eq nil (slot-value instance 'slot-2))
    (is eq :foo (slot-value instance 'slot-3))))

(define-test typechecked-class-makunbound :parent typechecked-class
  (let ((instance (make-instance 'test-typechecked-class
                                 :slot-2 nil :slot-3 :foo)))
    (fail (slot-makunbound instance 'slot-2) unbound-slot)
    (fail (slot-makunbound instance 'slot-3) unbound-slot)
    (fail (handler-bind ((unbound-slot (lambda (c) (store-value t c))))
            (slot-makunbound instance 'slot-2)) type-error)
    (fail (handler-bind ((unbound-slot (lambda (c) (store-value t c))))
            (slot-makunbound instance 'slot-3)) type-error)))
