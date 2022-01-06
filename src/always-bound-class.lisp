(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALWAYS-BOUND-CLASS

(defclass always-bound-class (standard-class) ())

(defmethod validate-superclass ((class always-bound-class)
                                (superclass standard-class))
  t)

(defmethod slot-makunbound-using-class
    ((class always-bound-class) object slotd)
  (if (not (slot-boundp-using-class class object slotd))
      (call-next-method)
      (let* ((old-value (slot-value-using-class class object slotd)))
        (unwind-protect
             (progn (call-next-method)
                    (setf (slot-value-using-class class object slotd)
                          (slot-value-using-class class object slotd)))
          (unless (slot-boundp-using-class class object slotd)
            (setf (slot-value-using-class class object slotd) old-value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALWAYS-BOUND-CLASS SHARED-INITIALIZE

(defun ensure-slots-bound (instance)
  (dolist (slot (class-slots (class-of instance)))
    (let ((name (slot-definition-name slot)))
      (unless (slot-boundp instance name)
        (setf (slot-value instance name)
              (slot-value instance name))))))

(defparameter *always-bound-initialize-mf*
  (compile nil (make-method-lambda
                (class-prototype (find-class 'standard-generic-function))
                (class-prototype (find-class 'standard-method))
                `(lambda (instance slot-names &rest args)
                   (declare (ignore slot-names args))
                   (ensure-slots-bound instance))
                nil)))

(defmethod shared-initialize :after ((class always-bound-class) slots &key)
  (let ((gf #'shared-initialize)
        (specializers (list class (find-class 't)))
        (qualifiers '(:after)))
    (a:when-let ((method (find-method gf qualifiers specializers nil)))
      (remove-method gf method))
    (let ((method (make-instance 'standard-method
                                 :specializers specializers
                                 :qualifiers qualifiers
                                 :lambda-list '(instance slot-names &rest args)
                                 :function *always-bound-initialize-mf*)))
      (add-method gf method))))
