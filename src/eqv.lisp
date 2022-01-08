(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defvar *eqv-state*)

(defun eqv (x y)
  (when (boundp '*eqv-state*)
    (error "Bug: EQV called again from inside EQV."))
  (let ((*eqv-state* (make-hash-table)))
    (eqv-using-class x y)))

(defgeneric eqv-using-class (x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EQ case and cycle handling

(defmethod eqv-using-class :around (x y)
  (or (eq x y)
      (multiple-value-bind (value foundp) (gethash x *eqv-state*)
        (cond (foundp (eql value y))
              (t (setf (gethash x *eqv-state*) y)
                 (call-next-method))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default method called - condition

(define-condition eqv-default-method-called (warning)
  ((args :reader eqv-default-method-called-args :initarg :args))
  (:default-initargs :args (a:required-argument :args))
  (:report (lambda (condition stream)
             (format stream "EQV default method called with ~S; ~
                             possible type error?"
                     (eqv-default-method-called-args condition)))))

(defvar *eqv-default-method-behavior* 'warn)

(defmethod eqv-using-class (x y)
  (when *eqv-default-method-behavior*
    (funcall *eqv-default-method-behavior*
             'eqv-default-method-called
             :args (list x y)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementations for standard Common Lisp types

(defmethod eqv-using-class ((x function) (y function)) (eq x y))

(defmethod eqv-using-class ((x symbol) (y symbol))
  (if (and (null (symbol-package x))
           (null (symbol-package y)))
      (string= x y)
      (eq x y)))

(defmethod eqv-using-class ((x package) (y package)) (eq x y))

(defmethod eqv-using-class ((x stream) (y stream)) (eq x y))

(defmethod eqv-using-class ((x number) (y number)) (= x y))

(defmethod eqv-using-class ((x character) (y character)) (char= x y))

(defmethod eqv-using-class ((x string) (y string)) (string= x y))

(defmethod eqv-using-class ((x pathname) (y pathname)) (equal x y))

(defmethod eqv-using-class ((x cons) (y cons))
  (and (eqv-using-class (car x) (car y))
       (eqv-using-class (cdr x) (cdr y))))

(defmethod eqv-using-class ((x array) (y array))
  (and (equal (array-dimensions x) (array-dimensions y))
       (loop for i below (array-total-size x)
             always (eqv-using-class (row-major-aref x i)
                                     (row-major-aref y i)))))

(defmethod eqv-using-class ((x hash-table) (y hash-table))
  (and (= (hash-table-count x) (hash-table-count y))
       (eq (hash-table-test x) (hash-table-test y))
       (flet ((compare (key x-value)
                (multiple-value-bind (y-value y-value-p) (gethash key y)
                  (unless (and y-value-p (eqv-using-class x-value y-value))
                    (return-from eqv-using-class nil)))))
         (maphash #'compare x)
         t)))
