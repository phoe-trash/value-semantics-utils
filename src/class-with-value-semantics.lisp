(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT-WITH-VALUE-SEMANTICS

(defclass object-with-value-semantics (standard-object) ())

(defmethod eqv-using-class ((x object-with-value-semantics)
                            (y object-with-value-semantics)
                            compare-fn fail-fn)
  (declare (type function compare-fn fail-fn))
  (declare (optimize speed))
  (let ((x-class (class-of x))
        (y-class (class-of y)))
    (unless (eq x-class y-class) (funcall fail-fn))
    (let* ((slots (u:slot-names x-class)))
      (labels ((continuation ()
                 (let* ((slot (car slots))
                        (boundp-x (slot-boundp x slot))
                        (boundp-y (slot-boundp y slot)))
                   (setf slots (cdr slots))
                   (cond
                     ;; Both slots bound.
                     ((and boundp-x boundp-y)
                      (funcall compare-fn
                               (slot-value x slot)
                               (slot-value y slot)
                               (when slots #'continuation)))
                     ;; Both slots unbound.
                     ((and (not boundp-x) (not boundp-y))
                      (funcall compare-fn nil nil
                               (when slots #'continuation)))
                     ;; All other cases: fail.
                     (t (funcall fail-fn))))))
        (continuation)))))

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
