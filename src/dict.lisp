(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dict

(defclass dict ()
  ((set :initarg :set :reader dict-set))
  (:metaclass class-with-value-semantics))

(defmethod initialize-instance :after
    ((dict dict) &key (set nil setp) (test #'eqv) (contents '()))
  (declare (ignore set))
  (unless setp
    (setf (slot-value dict 'set)
          (make-instance 'set :test test :contents (a:plist-alist contents)))))

(defmethod reinitialize-instance :after
    ((dict dict) &key (set nil setp) (test #'eqv) (contents '() contentsp))
  (declare (ignore set))
  (when (and (null setp) contentsp)
    (setf (slot-value dict 'set)
          (make-instance 'set :test test :contents (a:plist-alist contents)))))

(defun dict (&rest contents)
  (make-instance 'dict :set (apply #'set (a:plist-alist contents))))

(defmethod print-object ((object dict) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (set-contents (dict-set object)))))

(defun dict-test (dict) (set-test (dict-set dict)))

(defun dict-contents (dict) (set-contents (dict-set dict)))

(defun dict-count (dict) (set-count (dict-set dict)))

(defmethod generic-eqv ((x dict) (y dict))
  (generic-eqv (dict-set x) (dict-set y)))

(defun dict-insert (dict key value)
  (copy dict :set (set-insert (dict-set dict) (cons key value))))

(defun dict-remove (dict key &key ((:key key-fn) #'identity))
  (copy dict :set (set-remove (dict-set dict) key
                              :key (a:compose key-fn #'car))))

(defun dict-find (dict key &key ((:key key-fn) #'identity))
  (cdr (set-find (dict-set dict) key :key (a:compose key-fn #'car))))

(macrolet ((make (name operator)
             `(defun ,name (x y &key ((:key key-fn) #'identity))
                (let ((set (,operator (dict-set x)
                                      (dict-set y)
                                      :key (a:compose key-fn #'car))))
                  (copy x :set set)))))
  (make dict-difference set-difference)
  (make dict-union set-union)
  (make dict-intersection set-intersection)
  (make dict-exclusive-or set-exclusive-or))
