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
            (cond ((and boundp-x (not boundp-y)) (fail))
                  ((and boundp-y (not boundp-x)) (fail))
                  ((and boundp-x boundp-y
                        (not (eqv-using-class (slot-value x slot)
                                              (slot-value y slot))))
                   (fail))
                  ((and (not boundp-x) (not boundp-y))))))))))

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
