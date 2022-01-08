(in-package #:value-semantics-utils/test)

(define-test eqv :parent value-semantics-utils)

(define-test eqv-default-method :parent eqv
  (let ((conditions '()))
    (flet ((collect (condition) (push condition conditions)))
      (handler-bind ((vs:eqv-default-method-called #'collect))
        (let ((vs:*eqv-default-method-behavior* 'signal))
          (false (vs:eqv 42 "42")))
        (is = 1 (length conditions))
        (let ((condition (first conditions)))
          (is equal (42 "42") (vs:eqv-default-method-called-args condition)))
        (let ((vs:*eqv-default-method-behavior* 'nil))
          (false (vs:eqv 42 "42")))
        (is = 1 (length conditions))))))

(define-test eqv-cl-types-true :parent eqv
  (let ((vs:*eqv-default-method-behavior* 'error))
    (is vs:eqv #'car #'car)
    (is vs:eqv 'car 'car)
    (is vs:eqv :test :test)
    (is vs:eqv '#:foo '#:foo)
    (is vs:eqv (find-package '#:cl) (find-package '#:cl))
    (is vs:eqv *standard-output* *standard-output*)
    (is vs:eqv 42 42)
    (is vs:eqv 42 42.0)
    (is vs:eqv 42.0 42.0)
    (is vs:eqv #C(4 2) #C(4 2))
    (is vs:eqv #\C #\C)
    (is vs:eqv "Hello" "Hello")
    (is vs:eqv #P"foo/bar.baz" #P"foo/bar.baz")
    (is vs:eqv (cons 1 2) (cons 1 2))
    (is vs:eqv (list* 1 2 3 4 5) (list* 1 2 3 4 5))
    (is vs:eqv
        (list* (list "one" :two '#:three) 2 3 4 5)
        (list* (list "one" :two '#:three) 2 3 4 5))
    (flet ((cycle (length initial-element)
             (a:make-circular-list length :initial-element initial-element)))
      (is vs:eqv
          (list* 1 2 (cycle 3 3))
          (list* 1 2 (cycle 3 3)))
      (is vs:eqv
          (list* 1 2 (cycle 3 (cycle 30 3)))
          (list* 1 2 (cycle 3 (cycle 30 3)))))
    (is vs:eqv #(1 2 3 4 5) #(1 2 3 4 5))
    (is vs:eqv #2A((1 2 3) (4 5 6)) #2A((1 2 3) (4 5 6)))
    (is vs:eqv
        (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3)))
        (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3))))))

(define-test eqv-cl-types-false :parent eqv
  (let ((vs:*eqv-default-method-behavior* 'error))
    (isnt vs:eqv #'car #'cdr)
    (isnt vs:eqv 'car 'cdr)
    (isnt vs:eqv :test :key)
    (isnt vs:eqv '#:foo '#:bar)
    (isnt vs:eqv (find-package '#:cl) (find-package '#:cl-user))
    (isnt vs:eqv *standard-output* *standard-input*)
    (isnt vs:eqv 42 43)
    (isnt vs:eqv 42 43.0)
    (isnt vs:eqv 42.0 43.0)
    (isnt vs:eqv #C(4 2) #C(4 3))
    (isnt vs:eqv #\C #\D)
    (isnt vs:eqv #\C #\c)
    (isnt vs:eqv "Hello" "H")
    (isnt vs:eqv "Hello" "HELLO")
    (isnt vs:eqv #P"foo/bar.baz" #P"foo/bar.quux")
    (isnt vs:eqv (cons 1 2) (cons 1 0))
    (isnt vs:eqv (cons 1 2) (cons 0 2))
    (isnt vs:eqv (list* 1 2 3 4 5) (list* 1 2 0 4 5))
    (isnt vs:eqv (list* 1 2 3 4 5) (list* 1 2 3 4 0))
    (isnt vs:eqv
          (list* (list "one" :two '#:three) 2 3 4 5)
          (list* (list "one" :two '#:zero) 2 3 4 5))
    (flet ((cycle (length initial-element)
             (a:make-circular-list length :initial-element initial-element)))
      (isnt vs:eqv
            (list* 1 2 (cycle 3 4))
            (list* 1 2 (cycle 3 3)))
      (isnt vs:eqv
            (list* 1 2 (cycle 3 3))
            (list* 1 2 (cycle 4 3)))
      (isnt vs:eqv
            (list* 1 2 (cycle 3 (cycle 30 3)))
            (list* 1 2 (cycle 3 (cycle 30 4))))
      (isnt vs:eqv
            (list* 1 2 (cycle 3 (cycle 30 3)))
            (list* 1 2 (cycle 3 (cycle 40 3)))))
    (isnt vs:eqv #(1 2 3 4 5) #(1 2 3 4 5 6))
    (isnt vs:eqv #(1 2 3 4 5) #(1 2 3 0 5))
    (isnt vs:eqv #2A((1 2 3) (4 5 6)) #(1 2 3 4 5 6))
    (isnt vs:eqv #2A((1 2 3) (4 5 6)) #2A((1 2 0) (4 5 6)))
    (isnt vs:eqv #2A((1 2 3) (4 5 6)) #2A((1 2) (3 4) (5 6)))
    (isnt vs:eqv
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 0)))
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3))))
    (isnt vs:eqv
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3)))
          (a:alist-hash-table '((:a . 1) (:b . 2) (:zero . 3))))
    (isnt vs:eqv
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3)))
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3) (:d . 4))))
    (isnt vs:eqv
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3)))
          (a:alist-hash-table '((:a . 1) (:b . 2) (:c . 3))
                              :test 'equal))))

(defclass test-class () ())

(defvar *test-class-eqv*)

(defmethod vs:eqv-using-class ((x test-class) (y test-class))
  *test-class-eqv*)

(define-test eqv-custom-method :parent eqv
  (let ((x (make-instance 'test-class))
        (y (make-instance 'test-class)))
    (let ((*test-class-eqv* nil))
      (isnt vs:eqv x y))
    (let ((*test-class-eqv* t))
      (is vs:eqv x y))))
