(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GFs

(defgeneric generic-eqv (x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default method

(define-condition eqv-default-method-called (warning)
  ((args :reader eqv-default-method-called-args :initarg :args))
  (:default-initargs :args (a:required-argument :args))
  (:report (lambda (condition stream)
             (let ((*print-circle* t))
               (format stream "GENERIC-EQV: default method called ~
                               with ~S; possible type error?"
                       (eqv-default-method-called-args condition))))))

(defvar *eqv-default-method-behavior* 'warn)

(defmethod generic-eqv (x y)
  (cond ((eq x y)
         ;; In theory, we should invoke EQV-DEFAULT-METHOD-CALLED logic here,
         ;; but if two objects are EQ to each other then, by definition, they
         ;; are also EQV to each other.
         (values t nil nil nil))
        (t
         (when *eqv-default-method-behavior*
           (funcall *eqv-default-method-behavior*
                    'eqv-default-method-called
                    :args (list x y)))
         (values nil nil nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementations for standard Common Lisp types

(defmethod generic-eqv ((x function) (y function))
  (values (eq x y) nil nil nil))

(defmethod generic-eqv ((x symbol) (y symbol))
  (declare (optimize speed))
  (let ((result (or (eq x y)
                    (and (null (symbol-package x))
                         (null (symbol-package y))
                         (string= (symbol-name x) (symbol-name y))))))
    (values result nil nil nil)))

(defmethod generic-eqv ((x package) (y package))
  (values (eq x y) nil nil nil))

(defmethod generic-eqv ((x stream) (y stream))
  (values (eq x y) nil nil nil))

(defmethod generic-eqv ((x number) (y number))
  (values (= x y) nil nil nil))

(defmethod generic-eqv ((x character) (y character))
  (values (char= x y) nil nil nil))

(defmethod generic-eqv ((x string) (y string))
  (values (string= x y) nil nil nil))

(defmethod generic-eqv ((x pathname) (y pathname))
  (values (equal x y) nil nil nil))

(defmethod generic-eqv ((x cons) (y cons))
  (declare (optimize speed))
  (labels ((cdr-continuation () (values t (cdr x) (cdr y) nil))
           (car-continuation () (values t (car x) (car y) #'cdr-continuation)))
    (car-continuation)))

(defmethod generic-eqv ((x array) (y array))
  (declare (optimize speed))
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  ;; If the dimensions are different, the comparison fails.
  (unless (equal (array-dimensions x) (array-dimensions y))
    (return-from generic-eqv (values nil nil nil)))
  (let ((array-total-size (array-total-size x)))
    ;; If the arrays are empty, the comparison succeeds.
    (when (= 0 array-total-size)
      (return-from generic-eqv (values t nil nil nil)))
    ;; The arrays are not empty. Return a continuation that will compare them
    ;; element-wise.
    (let ((index 0))
      (declare (a:array-index index))
      (labels ((array-continuation ()
                 (let ((x-element (row-major-aref x index))
                       (y-element (row-major-aref y index)))
                   (incf index)
                   (let ((continuation (if (< index array-total-size)
                                           #'array-continuation
                                           nil)))
                     (values t x-element y-element continuation)))))
        (array-continuation)))))

(defmethod generic-eqv ((x hash-table) (y hash-table))
  (declare (optimize speed))
  ;; If the hash-table metadata is different, the comparison fails.
  (unless (and (= (hash-table-count x) (hash-table-count y))
               (eq (hash-table-test x) (hash-table-test y)))
    (return-from generic-eqv (values nil nil nil nil)))
  (let ((keys (alexandria:hash-table-keys x)))
    ;; If the hash-tables are empty, the comparison succeeds.
    (when (null keys)
      (return-from generic-eqv (values t nil nil nil)))
    ;; The hash-tables are not empty. Return a continuation that will compare
    ;; them key-and-value-wise.
    (labels ((hash-table-continuation ()
               (let* ((key (car keys))
                      (x-value (gethash key x)))
                 (setf keys (cdr keys))
                 (multiple-value-bind (y-value y-value-p) (gethash key y)
                   (unless y-value-p
                     (return-from hash-table-continuation
                       (values nil nil nil nil)))
                   (let ((continuation (if (not (null keys))
                                           #'hash-table-continuation
                                           nil)))
                     (return-from hash-table-continuation
                       (values t x-value y-value continuation)))))))
      (hash-table-continuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MERGE-CONTINUATIONS

(defun merge-continuations (earlier-cont later-cont)
  (declare (optimize speed))
  (cond
    ((null earlier-cont) later-cont)
    ((null later-cont) earlier-cont)
    (t (labels ((merged-continuation ()
                  ;; Compute the values from the earlier continuation.
                  (multiple-value-bind (successp x y mid-cont)
                      (funcall (the function earlier-cont))
                    (cond
                      ;; Optimization: if SUCCESSP is false, EQV is going to
                      ;; immediately quit anyway, so we can avoid computing
                      ;; the RESULT-CONT and simply return all NILs.
                      ((null successp)
                       (values nil nil nil nil))
                      ;; The earlier continuation halted. Return the later
                      ;; continuation object.
                      ((not mid-cont)
                       (values t x y later-cont))
                      ;; The earlier continuation did not halt. Remember the
                      ;; returned continuation object, forward the returned
                      ;; values, and return this merged continuation closure
                      ;; that will eventually return the later continuation.
                      (t
                       (setf earlier-cont mid-cont)
                       (values successp x y #'merged-continuation))))))
         #'merged-continuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EQV

(defun %eqv (start-continuation detect-cycles-p)
  (declare (optimize speed))
  (w:with-branching (detect-cycles-p)
    ;; Object equivalence owns the sky.
    ;; This thing can handle cycles on a dime,
    ;; continuation-passing style.
    (let ((continuation start-continuation)
          (state (w:branch-when detect-cycles-p
                   (make-hash-table :test #'eq))))
      (declare (type function continuation))
      ;; STATE is never accessed if DETECT-CYCLES-P is false.
      (declare (ignorable state))
      ;; Manual iteration time.
      (tagbody :start
         ;; Compute the new set of iteration values.
         (multiple-value-bind (result x y new-continuation)
             (funcall continuation)
           ;; Handle possible comparison failure.
           (when (null result) (return-from %eqv nil))
           ;; Handle possible comparison success.
           (when (and (null x) (null y) (null new-continuation))
             (return-from %eqv t))
           ;; If we're detecting cycles...
           (w:branch-when detect-cycles-p
             ;; ...then, have we already been here?
             (cond ((and x y (not (gethash y (a:ensure-gethash
                                              x state
                                              (make-hash-table :test #'eq)))))
                    ;; We haven't - remember that pair of objects for later.
                    (setf (gethash y (gethash x state)) t))
                   ;; OK, so we have already seen that pair of objects before.
                   ;; Is there a new continuation?
                   (new-continuation
                    ;; Then set it for processing in the next iteration
                    ;; and jump to it.
                    (setf continuation new-continuation)
                    (go :start))
                   ;; We've already compared these objects and there's no
                   ;; continuation to call. Nothing more to be done.
                   (t (return-from %eqv t))))
           ;; We assume there is no cycle. Where do we go now?
           (cond ((and x y)
                  ;; We have new values to compare.
                  (setf continuation
                        ;; If both values and a new continuation were returned,
                        ;; queue the continuation and compare the values first.
                        (merge-continuations (lambda () (generic-eqv x y))
                                             new-continuation)))
                 (t
                  ;; No new values  - use the new continuation as-is.
                  (setf continuation new-continuation)))
           ;; New continuation is set, time for a new iteration.
           (go :start))))))

(defun eqv (x y &key (detect-cycles-p t))
  (%eqv (lambda () (generic-eqv x y)) detect-cycles-p))
