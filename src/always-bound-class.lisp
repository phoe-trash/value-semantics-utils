(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALWAYS-BOUND-CLASS

(defclass always-bound-class (standard-class) ())

(defmethod validate-superclass ((class always-bound-class)
                                (superclass standard-class))
  t)

(defclass always-bound (standard-object) ())

(defmethod shared-initialize :around ((class always-bound-class) slot-names
                                      &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'always-bound)))
         rest))

(defun ensure-slot-bound (instance slotd)
  (let ((name (slot-definition-name slotd)))
    (unless (slot-boundp instance name)
      (setf (slot-value instance name) (slot-value instance name)))))

(defun ensure-slots-bound (instance)
  (dolist (slotd (class-slots (class-of instance)))
    (ensure-slot-bound instance slotd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLOT-MAKUNBOUND

(defmethod slot-makunbound-using-class
    ((class always-bound-class) (object always-bound) slotd)
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
;;; INITIALIZE-INSTANCE

(defmethod initialize-instance :after ((instance always-bound) &key)
  (ensure-slots-bound instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REINITIALIZE-INSTANCE

;;; REINITIALIZE-INSTANCE is specified in a way in which it can never make
;;; slots unbound - hence no implementation is needed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS

(defmethod update-instance-for-redefined-class :around
    ((instance always-bound)
     added-slots discarded-slots
     property-list
     &rest initargs)
  ;; If a slot already exists or is removed, we do not need to care about it.
  ;; If a slot is added, then we must ensure that it is going to be bound
  ;; or signal an error otherwise.
  (let* ((class (class-of instance))
         (slots (class-slots class)))
    (dolist (slot-name added-slots)
      (let ((slotd (find slot-name slots :key #'slot-definition-name)))
        (block continue
          ;; Does a provided initarg match this slot?
          (let ((slot-initargs (slot-definition-initargs slotd)))
            (dolist (slot-initarg slot-initargs)
              (when (get-properties initargs (list slot-initarg))
                (return-from continue))))
          ;; No initarg. Does the slot have an initfunction?
          (let ((initfunction (slot-definition-initfunction slotd)))
            (when initfunction
              (return-from continue)))
          ;; No initarg, no initfunction - this slot will be unbound if we
          ;; proceed. Ensure it is bound before continuing.
          (ensure-slot-bound instance slotd)))))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS

;; (defparameter *always-bound-initialize-mf*
;;   (compile nil (make-method-lambda
;;                 (class-prototype (find-class 'standard-generic-function))
;;                 (class-prototype (find-class 'standard-method))
;;                 `(lambda (instance slot-names &rest args)
;;                    (declare (ignore slot-names args))
;;                    (ensure-slots-bound instance))
;;                 nil)))

;; (defmethod shared-initialize :after ((class always-bound-class) slots &key)
;;   (let ((gf #'shared-initialize)
;;         (specializers (list class (find-class 't)))
;;         (qualifiers '(:after)))
;;     (a:when-let ((method (find-method gf qualifiers specializers nil)))
;;       (remove-method gf method))
;;     (let ((method (make-instance 'standard-method
;;                                  :specializers specializers
;;                                  :qualifiers qualifiers
;;                                  :lambda-list '(instance slot-names &rest args)
;;                                  :function *always-bound-initialize-mf*)))
;;       (add-method gf method))))
