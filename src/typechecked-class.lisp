(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPECHECKED-CLASS

(defclass typechecked-class (always-bound-class) ())

(defmethod validate-superclass ((c typechecked-class) (s standard-class))
  t)

(defclass typechecked-slot-definition (standard-slot-definition) ())

(defclass typechecked-effective-slot-definition
    (standard-effective-slot-definition typechecked-slot-definition)
  ((%slot-definition-typecheck-function
    :accessor slot-definition-typecheck-function)))

(defmethod effective-slot-definition-class ((c typechecked-class) &rest args)
  (declare (ignore args))
  (find-class 'typechecked-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class typechecked-class) name dsds)
  (let* ((esd (call-next-method))
         (type (slot-definition-type esd))
         (typecheck-function
           (compile nil `(lambda (,name) (check-type ,name ,type) ,name))))
    (setf (slot-definition-typecheck-function esd) typecheck-function)
    esd))

(defmethod (setf slot-value-using-class) :around
    (new-value (class typechecked-class) object
     (slot typechecked-effective-slot-definition))
  (setf new-value (funcall (slot-definition-typecheck-function slot) new-value))
  (call-next-method new-value class object slot))
