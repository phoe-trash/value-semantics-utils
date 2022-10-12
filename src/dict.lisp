(in-package #:value-semantics-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dict

(defclass dict ()
  ((set :initarg :set :reader dict-set))
  (:metaclass class-with-value-semantics))

(defmethod shared-initialize :after
    ((dict dict) slots
     &key (set nil setp) (test #'eqv) (contents '() contentsp))
  ;; TODO test SET and DICT for deduplication
  (declare (ignore set))
  (when (and (null setp) contentsp)
    (setf (slot-value dict 'set)
          (make-instance 'set :test test :contents (a:plist-alist contents))))
  (let* ((set (dict-set dict))
         (test (set-test set))
         (contents-1 (set-contents set))
         (contents-2 (remove-duplicates contents-1 :key #'car :test test)))
    (unless (= (length contents-1) (length contents-2))
      (setf (slot-value set 'contents) contents-2
            (slot-value set 'count) (length contents-2)))))

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

(defun dict-map (function set)
  (dolist (element (dict-contents set))
    (funcall function (car element) (cdr element))))

(macrolet ((make (name operator)
             `(defun ,name (x y &key ((:key key-fn) #'identity))
                (let ((set (,operator (dict-set x)
                                      (dict-set y)
                                      :key (a:compose key-fn #'car))))
                  (copy x :set set)))))
  (make dict-union set-union)
  (make dict-intersection set-intersection)
  (make dict-difference set-difference)
  (make dict-exclusive-or set-exclusive-or))

(defun dict-union* (x y &key (key #'identity))
  (dict-union x (dict-difference y x :key key) :key key))

(defun dict-intersection* (x y &key (key #'identity))
  (dict-difference x (dict-exclusive-or x y :key key) :key key))
