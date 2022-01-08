(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT-WITH-VALUE-SEMANTICS

(defclass object-with-value-semantics (standard-object) ())

(defmethod eqv-using-class ((x object-with-value-semantics)
                            (y object-with-value-semantics))
  (declare (optimize speed))
  (flet ((fail () (return-from eqv-using-class nil)))
    (let ((x-class (class-of x))
          (y-class (class-of y)))
      (unless (eq x-class y-class) (fail))
      (let* ((slots (u:slot-names x-class)))
        (dolist (slot slots t)
          (let ((boundp-x (slot-boundp x slot))
                (boundp-y (slot-boundp y slot)))
            (cond
              ;; Both slots bound with EQV values.
              ((and boundp-x boundp-y
                    (eqv-using-class (slot-value x slot)
                                     (slot-value y slot))))
              ;; Both slots unbound.
              ((and (not boundp-x) (not boundp-y)))
              ;; All other cases are not EQV.
              (t (fail)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASS-WITH-VALUE-SEMANTICS

(defclass class-with-value-semantics (standard-class) ())

(defmethod validate-superclass ((class class-with-value-semantics)
                                (superclass standard-class))
  t)

(defmethod shared-initialize :around
    ((class class-with-value-semantics) slot-names
     &rest rest &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'object-with-value-semantics)))
         rest))
