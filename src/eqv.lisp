(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GFs

(defgeneric eqv-using-class (x y compare-fn fail-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default method called - condition

(define-condition eqv-default-method-called (warning)
  ((args :reader eqv-default-method-called-args :initarg :args))
  (:default-initargs :args (a:required-argument :args))
  (:report (lambda (condition stream)
             (format stream "EQV-USING-CLASS: default method called ~
                             with ~S; possible type error?"
                     (eqv-default-method-called-args condition)))))

(defvar *eqv-default-method-behavior* 'warn)

(defmethod eqv-using-class (x y compare-fn fail-fn)
  (when *eqv-default-method-behavior*
    (funcall *eqv-default-method-behavior*
             'eqv-default-method-called
             :args (list x y)))
  (funcall fail-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementations for standard Common Lisp types

(defmethod eqv-using-class ((x function) (y function) compare-fn fail-fn)
  (unless (eq x y) (funcall fail-fn)))

(defmethod eqv-using-class ((x symbol) (y symbol) compare-fn fail-fn)
  (unless (or (eq x y)
              (and (null (symbol-package x))
                   (null (symbol-package y))
                   (string= (symbol-name x) (symbol-name y))))
    (funcall fail-fn)))

(defmethod eqv-using-class ((x package) (y package) compare-fn fail-fn)
  (unless (eq x y) (funcall fail-fn)))

(defmethod eqv-using-class ((x stream) (y stream) compare-fn fail-fn)
  (unless (eq x y) (funcall fail-fn)))

(defmethod eqv-using-class ((x number) (y number) compare-fn fail-fn)
  (unless (= x y) (funcall fail-fn)))

(defmethod eqv-using-class ((x character) (y character) compare-fn fail-fn)
  (unless (char= x y) (funcall fail-fn)))

(defmethod eqv-using-class ((x string) (y string) compare-fn fail-fn)
  (unless (string= x y) (funcall fail-fn)))

(defmethod eqv-using-class ((x pathname) (y pathname) compare-fn fail-fn)
  (unless (equal x y) (funcall fail-fn)))

(defmethod eqv-using-class
    ((x cons) (y cons) compare-fn fail-fn)
  (labels ((cdr-continuation () (funcall compare-fn (cdr x) (cdr y)))
           (car-continuation () (funcall compare-fn (car x) (car y)
                                         #'cdr-continuation)))
    (car-continuation)))

(defmethod eqv-using-class ((x array) (y array) compare-fn fail-fn)
  (declare (type function compare-fn fail-fn))
  (declare (optimize speed))
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (unless (equal (array-dimensions x) (array-dimensions y))
    (funcall fail-fn))
  (let ((index 0)
        (array-total-size (array-total-size x)))
    (declare (a:array-index index))
    (when (plusp array-total-size)
      (labels ((continuation ()
                 (let ((i index))
                   (incf index)
                   (funcall compare-fn (row-major-aref x i) (row-major-aref y i)
                            (when (< index array-total-size)
                              #'continuation)))))
        (continuation)))))

(defmethod eqv-using-class ((x hash-table) (y hash-table) compare-fn fail-fn)
  (declare (type function compare-fn fail-fn))
  (declare (optimize speed))
  (unless (and (= (hash-table-count x) (hash-table-count y))
               (eq (hash-table-test x) (hash-table-test y)))
    (funcall fail-fn))
  ;; We cannot close over WITH-HASH-TABLE-ITERATOR as it has dynamic extent.
  (alexandria:when-let ((keys (alexandria:hash-table-keys x)))
    (labels ((continuation ()
               (let* ((key (car keys))
                      (x-value (gethash key x)))
                 (setf keys (cdr keys))
                 (multiple-value-bind (y-value y-value-p) (gethash key y)
                   (unless y-value-p (funcall fail-fn))
                   (funcall compare-fn x-value y-value
                            (when keys
                              #'continuation))))))
      (continuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Holy shit I wrote this thing but I have no idea what or how it does help me

(defun eqv (x y &key (detect-cycles-p t))
  (declare (optimize speed))
  (w:with-macroexpand-time-branching (detect-cycles-p)
    (let ((continuation nil)
          (state (w:macroexpand-time-when detect-cycles-p
                   (make-hash-table :test #'eq))))
      ;; STATE is never accessed if DETECT-CYCLES-P is false.
      (declare (ignorable state))
      (labels ((fail () (return-from eqv nil))
               (compare (x y &optional thunk)
                 (declare (type (or null function) thunk))
                 (flet ((stack-frame ()
                          (w:macroexpand-time-when detect-cycles-p
                            ;; Have we already visited this pair of objects?
                            (when (member y (gethash x state) :test #'eq)
                              ;; We have - call the thunk, then bail out.
                              (when thunk (funcall thunk))
                              (return-from stack-frame))
                            ;; We haven't - store the objects in the hashtable.
                            (pushnew y (gethash x state) :test #'eq))
                          ;; Defer the actual comparison to EQV-USING-CLASS
                          ;; and call the thunk.
                          (eqv-using-class x y #'compare #'fail)
                          (when thunk (funcall thunk))))
                   (let ((old-continuation continuation))
                     (setf continuation
                           (if old-continuation
                               ;; We must store the old continuation and
                               ;; call it before we call the new stack frame.
                               (lambda ()
                                 (funcall (the function old-continuation))
                                 (funcall #'stack-frame))
                               #'stack-frame))))))
        ;; Handle the starting elements.
        (compare x y)
        ;; Loop until there is no continuation.
        (loop until (null continuation)
              for old-continuation = continuation
              do (setf continuation nil)
                 (funcall (the function old-continuation)))
        ;; If there was no non-local exit, the match is complete.
        t))))
