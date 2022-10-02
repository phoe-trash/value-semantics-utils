(in-package #:value-semantics-utils/test)

(defclass test-class-with-value-semantics ()
  ((slot-1 :initarg :slot-1)
   (slot-2 :initarg :slot-2))
  (:metaclass vs:class-with-value-semantics))

(define-test class-with-value-semantics :parent value-semantics-utils)

(defparameter *class-with-value-semantics-thunk*
  `(lambda ()
     (defclass other-test-class (test-class-with-value-semantics) ()
       (:metaclass standard-class))))

(define-test class-with-value-semantics-validate-superclass-failure
  :parent class-with-value-semantics
  (let ((thunk (compile nil *class-with-value-semantics-thunk*)))
    (fail (funcall thunk))))

(define-test class-with-value-semantics-true :parent class-with-value-semantics
  (flet ((make (&rest args)
           (apply #'make-instance 'test-class-with-value-semantics args)))
    (let ((x (make))
          (y (make)))
      (is vs:eqv x y))
    (let ((x (make :slot-1 1))
          (y (make :slot-1 1)))
      (is vs:eqv x y))
    (let ((x (make :slot-2 2))
          (y (make :slot-2 2)))
      (is vs:eqv x y))
    (let ((x (make :slot-1 1 :slot-2 2))
          (y (make :slot-1 1 :slot-2 2)))
      (is vs:eqv x y))
    (let* ((x (make))
           (y (make)))
      (setf (slot-value x 'slot-1) y
            (slot-value y 'slot-1) x)
      (is vs:eqv x y))))

(define-test class-with-value-semantics-false :parent class-with-value-semantics
  (flet ((make (&rest args)
           (apply #'make-instance 'test-class-with-value-semantics args)))
    (let ((x (make :slot-1 1))
          (y (make)))
      (isnt vs:eqv x y))
    (let ((x (make :slot-1 1))
          (y (make :slot-1 2)))
      (isnt vs:eqv x y))
    (let ((x (make :slot-1 1))
          (y (make :slot-2 1)))
      (isnt vs:eqv x y))
    (let ((x (make))
          (y (make :slot-1 1 :slot-2 2)))
      (isnt vs:eqv x y))
    (let ((x (make :slot-1 1))
          (y (make :slot-1 1 :slot-2 2)))
      (isnt vs:eqv x y))
    (let ((x (make :slot-1 1 :slot-2 2))
          (y (make :slot-1 1 :slot-2 3)))
      (isnt vs:eqv x y))
    (let* ((x (make))
           (y (make :slot-2 42)))
      (setf (slot-value x 'slot-1) x
            (slot-value y 'slot-1) x)
      (isnt vs:eqv x y))))

(defclass other-test-class-with-value-semantics ()
  ((slot-1 :initarg :slot-1)
   (slot-2 :initarg :slot-2))
  (:metaclass vs:class-with-value-semantics))

(define-test class-with-value-semantics-different-classes
  :parent class-with-value-semantics
  (let ((instance-1 (make-instance 'test-class-with-value-semantics
                                   :slot-1 42 :slot-2 24))
        (instance-2 (make-instance 'other-test-class-with-value-semantics
                                   :slot-1 42 :slot-2 24)))
    (isnt vs:eqv instance-1 instance-2)))

(define-test class-with-value-semantics-copy
  :parent class-with-value-semantics
  (let* ((instance-1 (make-instance 'test-class-with-value-semantics
                                    :slot-1 42 :slot-2 24))
         (instance-2 (vs:copy instance-1))
         (instance-3 (vs:copy instance-1 :slot-1 1000)))
    (is = 1000 (slot-value instance-3 'slot-1))
    (is = 24 (slot-value instance-3 'slot-2))
    (is vs:eqv instance-1 instance-2)
    (isnt vs:eqv instance-1 instance-3)))
