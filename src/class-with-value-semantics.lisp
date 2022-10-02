(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT-WITH-VALUE-SEMANTICS

(defclass object-with-value-semantics (standard-object) ())

(defmethod generic-eqv ((x object-with-value-semantics)
                        (y object-with-value-semantics))
  (declare (optimize speed))
  (let ((x-class (class-of x))
        (y-class (class-of y)))
    ;; If object classes are different, the comparison fails.
    (unless (eq x-class y-class)
      (return-from generic-eqv (values nil nil nil nil)))
    (let* ((slots (u:slot-names x-class)))
      ;; If the instances have no slots, the comparison succeeds.
      (when (null slots)
        (return-from generic-eqv (values t nil nil nil)))
      ;; There are slots present in the instances. Return a continuation that
      ;; will compare them value-wise.
      (labels ((value-semantics-continuation ()
                 (let* ((slot (car slots))
                        (boundp-x (slot-boundp x slot))
                        (boundp-y (slot-boundp y slot)))
                   (setf slots (cdr slots))
                   (let ((continuation (if (not (null slots))
                                           #'value-semantics-continuation
                                           nil)))
                     (cond
                       ;; Both slots bound.
                       ((and boundp-x boundp-y)
                        (values t
                                (slot-value x slot)
                                (slot-value y slot)
                                continuation))
                       ;; Both slots unbound.
                       ((and (not boundp-x) (not boundp-y))
                        (values t nil nil continuation))
                       ;; All other cases: fail.
                       (t (values nil nil nil nil)))))))
        (value-semantics-continuation)))))

(defgeneric copy (original &rest initargs)
  (:documentation "Performs copying of the original object.")
  (:method ((original object-with-value-semantics) &rest initargs)
    (let* ((class (class-of original))
           (copy (allocate-instance class))
           (slots (class-slots class))
           (slot-names (mapcar #'slot-definition-name slots)))
      (dolist (slot slot-names)
        (when (slot-boundp original slot)
          (setf (slot-value copy slot) (slot-value original slot))))
      (apply #'reinitialize-instance copy initargs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASS-WITH-VALUE-SEMANTICS

(defclass class-with-value-semantics (standard-class) ())

(defmethod validate-superclass ((class class-with-value-semantics)
                                (superclass standard-class))
  t)

;; TODO methods on SHARED-INITIALIZE on classes are not portable
(defmethod shared-initialize :around
    ((class class-with-value-semantics) slot-names
     &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'object-with-value-semantics)))
         rest))
