(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPECHECKED-OBJECT

(defclass typechecked-object (always-bound-object) ())

(defmethod shared-initialize :after
    ((object typechecked-object) slot-names &rest initargs)
  (declare (ignore initargs))
  (let ((class (class-of object)))
    (dolist (slotd (class-slots (class-of object)))
      (let ((type (slot-definition-type slotd))
            (value (slot-value-using-class class object slotd)))
        (assert (typep value type)
                ((slot-value-using-class class object slotd))
                'type-error :datum value :expected-type type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPECHECKED-CLASS

(defclass typechecked-class (always-bound-class) ())

(defmethod validate-superclass ((class typechecked-class)
                                (superclass standard-class))
  t)

(defmethod shared-initialize :around ((class typechecked-class) slot-names
                                      &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'typechecked-object)))
         rest))

(defmethod (setf slot-value-using-class) :around
    (new-value (class typechecked-class) object slotd)
  (let ((type (slot-definition-type slotd)))
    (assert (typep new-value type) (new-value)
            'type-error :datum new-value :expected-type type)
    (call-next-method new-value class object slotd)))
