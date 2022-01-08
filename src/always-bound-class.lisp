(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALWAYS-BOUND

(defclass always-bound-object (standard-object) ())

(defmethod shared-initialize :after
    ((object always-bound-object) slot-names &rest initargs)
  (declare (ignore initargs))
  (let ((class (class-of object)))
    (dolist (slotd (class-slots (class-of object)))
      (unless (slot-boundp-using-class class object slotd)
        (setf (slot-value-using-class class object slotd)
              (slot-value-using-class class object slotd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALWAYS-BOUND-CLASS

(defclass always-bound-class (standard-class) ())

(defmethod validate-superclass ((class always-bound-class)
                                (superclass standard-class))
  t)

(defmethod shared-initialize :around ((class always-bound-class) slot-names
                                      &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'always-bound-object)))
         rest))

(defmethod slot-makunbound-using-class :around
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
