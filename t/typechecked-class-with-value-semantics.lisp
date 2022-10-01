(in-package #:value-semantics-utils/test)

(defclass test-typechecked-class-with-value-semantics ()
  ((slot-1 :initarg :slot-1
           :initform 42
           :type integer)
   (slot-2 :initarg :slot-2
           :type null)
   (slot-3 :initarg :slot-3
           :type keyword))
  (:metaclass vs:typechecked-class-with-value-semantics))

(define-test typechecked-class-with-value-semantics
  :parent value-semantics-utils)

(defparameter *typechecked-class-with-value-semantics-thunk*
  `(lambda ()
     (defclass other-test-class (test-typechecked-class-with-value-semantics) ()
       (:metaclass standard-class))))

(define-test typechecked-class-with-value-semantics-validate-superclass-failure
  :parent typechecked-class-with-value-semantics
  (let ((thunk (compile nil *always-bound-class-thunk*)))
    (fail (funcall thunk))))

(define-test typechecked-class-with-value-semantics-fail
  :parent typechecked-class-with-value-semantics
  (fail (make-instance 'test-typechecked-class-with-value-semantics)
      'unbound-slot)
  (fail (make-instance 'test-typechecked-class-with-value-semantics
                       :slot-2 nil)
      'unbound-slot))

(define-test typechecked-class-with-value-semantics-use-value
  :parent typechecked-class-with-value-semantics
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (use-value :foo c))))
      (setf instance
            (make-instance 'test-typechecked-class-with-value-semantics
                           :slot-2 nil))
      (is eq :foo (slot-value instance 'slot-3)))))

(define-test typechecked-class-with-value-semantics-store-value
  :parent typechecked-class-with-value-semantics
  (let (instance)
    (handler-bind ((unbound-slot (lambda (c) (store-value :foo c))))
      (setf instance
            (make-instance 'test-typechecked-class-with-value-semantics
                           :slot-2 nil))
      (is eq :foo (slot-value instance 'slot-3)))))

(define-test typechecked-class-with-value-semantics-setf-slot-value
  :parent typechecked-class-with-value-semantics
  (let ((instance (make-instance 'test-typechecked-class-with-value-semantics
                                 :slot-2 nil :slot-3 :foo)))
    (is = 42 (slot-value instance 'slot-1))
    (setf (slot-value instance 'slot-1) 24)
    (is = 24 (slot-value instance 'slot-1))))

(define-test typechecked-class-with-value-semantics-slot-makunbound
  :parent typechecked-class-with-value-semantics
  (let ((instance (make-instance 'test-typechecked-class-with-value-semantics
                                 :slot-2 nil :slot-3 :foo)))
    (handler-bind ((unbound-slot (lambda (c) (store-value 0 c))))
      (slot-makunbound instance 'slot-1))
    (is = 0 (slot-value instance 'slot-1))))

(define-test typechecked-class-with-value-semantics-reinitialize-instance
  :parent typechecked-class-with-value-semantics
  (let ((instance (make-instance 'test-typechecked-class-with-value-semantics
                                 :slot-1 24 :slot-2 nil :slot-3 :foo)))
    (reinitialize-instance instance :slot-3 :bar)
    (is = 24 (slot-value instance 'slot-1))
    (is eq :bar (slot-value instance 'slot-3))))

(define-test
    typechecked-class-with-value-semantics-update-instance-for-redefined-class
  :parent typechecked-class-with-value-semantics
  (unwind-protect
       (progn
         (let (instance)
           (defclass test-typechecked-class-with-value-semantics-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1 :type integer))
             (:metaclass vs:typechecked-class-with-value-semantics))
           (setf instance
                 (make-instance
                  'test-typechecked-class-with-value-semantics-u-i-f-r-c
                  :slot-1 42))
           (defclass test-typechecked-class-with-value-semantics-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1 :type integer)
              (slot-2 :initarg :slot-2))
             (:metaclass vs:typechecked-class-with-value-semantics))
           (fail (slot-value instance 'slot-1) unbound-slot)
           (fail (slot-value instance 'slot-1) unbound-slot)
           (defclass test-typechecked-class-with-value-semantics-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1 :type integer)
              (slot-2 :initarg :slot-2 :initform 24))
             (:metaclass vs:typechecked-class-with-value-semantics))
           (is = 42 (slot-value instance 'slot-1))
           (is = 24 (slot-value instance 'slot-2))
           (defclass test-typechecked-class-with-value-semantics-u-i-f-r-c ()
             ((slot-1 :initarg :slot-1 :type integer)
              (slot-2 :initarg :slot-2 :initform 24)
              (slot-3 :initarg :slot-3))
             (:metaclass vs:typechecked-class-with-value-semantics))
           (handler-bind ((unbound-slot (lambda (c) (store-value 0 c))))
             (slot-value instance 'slot-1))
           (is = 42 (slot-value instance 'slot-1))
           (is = 24 (slot-value instance 'slot-2))
           (is = 0 (slot-value instance 'slot-3))))
    (setf (find-class 'test-typechecked-class-with-value-semantics-u-i-f-r-c
                      nil)
          nil)))

(defclass simple-class ()
  ((other-slot :initform 42)))

(define-test
    typechecked-class-with-value-semantics-update-instance-for-different-class
  :parent typechecked-class-with-value-semantics
  (let ((instance (make-instance 'simple-class)))
    (fail (change-class instance 'test-typechecked-class-with-value-semantics)
        unbound-slot)
    (fail (change-class instance 'test-typechecked-class-with-value-semantics
                        :slot-2 t :slot-3 :foo)
        type-error)
    (true (typep instance 'simple-class))
    (is = 42 (slot-value instance 'other-slot))
    (change-class instance 'test-typechecked-class-with-value-semantics
                  :slot-2 nil :slot-3 :foo)
    (true (typep instance 'test-typechecked-class-with-value-semantics))
    (is eq nil (slot-value instance 'slot-2))
    (is eq :foo (slot-value instance 'slot-3))))

(define-test typechecked-class-with-value-semantics-eqv
  :parent typechecked-class-with-value-semantics
  (let ((instance-1 (make-instance 'test-typechecked-class-with-value-semantics
                                   :slot-2 nil :slot-3 :foo))
        (instance-2 (make-instance 'test-typechecked-class-with-value-semantics
                                   :slot-2 nil :slot-3 :foo))
        (instance-3 (make-instance 'test-typechecked-class-with-value-semantics
                                   :slot-2 nil :slot-3 :bar)))
    (is vs:eqv instance-1 instance-2)
    (isnt vs:eqv instance-1 instance-3)
    (isnt vs:eqv instance-2 instance-3)))
