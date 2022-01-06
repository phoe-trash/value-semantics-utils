(in-package #:value-semantics-utils/test)

(defclass test-typechecked-class ()
  ((slot-1 :initarg :slot-1
           :initform 42
           :type integer)
   (slot-2 :initarg :slot-2
           :type null)
   (slot-3 :initarg :slot-3
           :type keyword))
  (:metaclass vs:typechecked-class))

;;; TODO
