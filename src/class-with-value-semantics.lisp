(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASS-WITH-VALUE-SEMANTICS

(defclass class-with-value-semantics (standard-class) ())

(defmethod validate-superclass ((class class-with-value-semantics)
                                (superclass standard-class))
  t)

(defun slot-based-eqv (x y)
  (declare (optimize speed))
  (flet ((fail () (return-from slot-based-eqv nil)))
    (let* ((slots (mopu:slot-names (class-of x))))
      (dolist (slot slots t)
        (let ((boundp-x (slot-boundp x slot))
              (boundp-y (slot-boundp y slot)))
          (cond ((and boundp-x (not boundp-y)) (fail))
                ((and boundp-y (not boundp-x)) (fail))
                ((and boundp-x boundp-y
                      (not (eqv-using-class (slot-value x slot)
                                            (slot-value y slot))))
                 (fail))
                ((and (not boundp-x) (not boundp-y)))))))))

(defparameter *value-semantics-method-function*
  (compile nil (make-method-lambda
                (class-prototype (find-class 'standard-generic-function))
                (class-prototype (find-class 'standard-method))
                '(lambda (x y) (slot-based-eqv x y))
                nil)))

(defmethod shared-initialize :after
    ((class class-with-value-semantics) slots &key)
  (let ((gf #'eqv-using-class)
        (specializers (list class class)))
    (a:when-let ((method (find-method gf nil specializers nil)))
      (remove-method gf method))
    (let ((method (make-instance 'standard-method
                                 :specializers specializers
                                 :lambda-list '(x y)
                                 :function *value-semantics-method-function*)))
      (add-method gf method))))
