(in-package #:value-semantics-utils/test)

(defclass test-always-bound-class ()
  ((slot :initarg :slot))
  (:metaclass vs:always-bound-class))

(define-test always-bound-class :parent value-semantics-utils)

(defparameter *always-bound-class-thunk*
  `(lambda ()
     (defclass other-test-class (test-class-with-value-semantics) ()
       (:metaclass standard-class))))

(define-test always-bound-classvalidate-superclass-failure
  :parent always-bound-class
  (let ((thunk (compile nil *always-bound-class-thunk*)))
    (fail (funcall thunk))))

(define-test always-bound-class-fail :parent always-bound-class
  (fail (make-instance 'test-always-bound-class) 'unbound-slot))

(define-test always-bound-class-use-value :parent always-bound-class
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (use-value 42 c))))
      (setf instance (make-instance 'test-always-bound-class))
      (is = 42 (slot-value instance 'slot))
      (true (typep instance 'vs:always-bound-object)))))

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

(define-test always-bound-class-update-instance-for-redefined-class
  :parent always-bound-class
  (unwind-protect
       (progn
         (let (instance)
           (defclass test-always-bound-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1))
             (:metaclass vs:always-bound-class))
           (setf instance (make-instance 'test-always-bound-class-u-i-f-r-c
                                         :slot-1 42))
           (defclass test-always-bound-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1)
              (slot-2 :initarg :slot-2))
             (:metaclass vs:always-bound-class))
           (fail (slot-value instance 'slot-1) unbound-slot)
           (fail (slot-value instance 'slot-1) unbound-slot)
           (defclass test-always-bound-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1)
              (slot-2 :initarg :slot-2 :initform 24))
             (:metaclass vs:always-bound-class))
           (is = 42 (slot-value instance 'slot-1))
           (is = 24 (slot-value instance 'slot-2))
           (defclass test-always-bound-class-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1)
              (slot-2 :initarg :slot-2 :initform 24)
              (slot-3 :initarg :slot-3))
             (:metaclass vs:always-bound-class))
           (handler-bind ((unbound-slot (lambda (c) (store-value 0 c))))
             (slot-value instance 'slot-1))
           (is = 42 (slot-value instance 'slot-1))
           (is = 24 (slot-value instance 'slot-2))
           (is = 0 (slot-value instance 'slot-3))))
    (setf (find-class 'test-always-bound-class-u-i-f-r-c nil) nil)))

(defclass simple-class ()
  ((other-slot :initform 42)))

(define-test always-bound-class-update-instance-for-different-class
  :parent always-bound-class
  (let ((instance (make-instance 'simple-class)))
    (fail (change-class instance 'test-always-bound-class) unbound-slot)
    (true (typep instance 'simple-class))
    (is = 42 (slot-value instance 'other-slot))
    (change-class instance 'test-always-bound-class
                  :slot 24)
    (true (typep instance 'test-always-bound-class))
    (is = 24 (slot-value instance 'slot))))
