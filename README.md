# VALUE-SEMANTICS-UTILS

Utilities for adjusting CLOS for mostly-functional programming, including:


* **`eqv`**, an equivalence predicate that acts mostly like `equal`
  except it is extensible and does not hang on cycles;
* **`eqv-using-class`**, a means of programming `eqv`;
* **`class-with-value-semantics`**, a metaclass which automatically adds
  `eqv-using-class` methods specialized on the class being defined;
* **`always-bound-class`**, a metaclass whose instances cannot have their
  slots unbound at any time;
* **`typechecked-class`**, an `always-bound-class` with mandatory runtime
  typechecking for slot values;
  * **`typechecked-slot-definition`** and 
    **`typechecked-effective-slot-definition`**,
    slot definition classes for typechecked slots;
  * **`slot-definition-typecheck-function`**, an accessor function for the
    typecheck function of a typechecked slot;
* **`typechecked-class-with-value-semantics`**, a composition of the above three
  metaclasses.

## TODO

* Rationale and examples and manual
* Handle and test for `reinitialize-instance`
* Handle and test for `update-instance-for-redefined-class`
* Handle and test for `update-instance-for-different-class`
* Wait for https://bugs.launchpad.net/sbcl/+bug/1956621 to get fixed and
  unskip a test that depends on it

## License

MIT.

## Fixups

Until SBCL catches up, evaluate the following in order to get the tests to pass.
Trust me, I'm an engineer.

```lisp

(in-package #:sb-pcl)

(sb-ext:without-package-locks
  (defun slot-makunbound (object slot-name)
    (let* ((class (class-of object))
           (slotd (find slot-name (class-slots class)
                        :key #'slot-definition-name))
           (actual-methods (compute-applicable-methods
                            #'slot-makunbound-using-class
                            (list class object slotd)))
           (actual-method (first actual-methods))
           (original-methods
             (list (find-method
                    #'slot-makunbound-using-class nil
                    (list (find-class 'std-class)
                          (find-class 'standard-object)
                          (find-class 'standard-effective-slot-definition)))
                   (find-method
                    #'slot-makunbound-using-class nil
                    (list (find-class 'condition-class)
                          (find-class 't)
                          (find-class 't)))
                   (find-method
                    #'slot-makunbound-using-class nil
                    (list (find-class 'structure-class)
                          (find-class 'structure-object)
                          (find-class 'structure-effective-slot-definition))))))
      (unless (member actual-method original-methods)
        (return-from slot-makunbound
          (slot-makunbound-using-class class object slotd))))
    (let* ((wrapper (valid-wrapper-of object))
           (cell (find-slot-cell wrapper slot-name))
           (location (car cell)))
      (cond ((fixnump location)
             (if (std-instance-p object)
                 (setf (standard-instance-access object location) +slot-unbound+)
                 (setf (funcallable-standard-instance-access object location)
                       +slot-unbound+)))
            ((not location)
             (if cell
                 (let ((class (wrapper-class wrapper)))
                   (slot-makunbound-using-class class object
                                                (find-slot-definition class slot-name)))
                 (slot-missing (wrapper-class wrapper) object slot-name
                               'slot-makunbound)))
            ((listp location) ; forcibly transform CONSP to LISTP
             (setf (cdr location) +slot-unbound+))
            (t
             (bug "Bogus slot-cell in SLOT-MAKUNBOUND: ~S" cell))))
    object))

(defmacro wrapper-class (wrapper)
  `(classoid-pcl-class (wrapper-classoid ,wrapper)))

(macrolet ((replace-wrapper-and-slots (thing layout slot-vector)
             `(if (functionp ,thing)
                  (setf (%fun-wrapper ,thing) ,layout
                        (fsc-instance-slots ,thing) ,slot-vector)
                  ;; TODO: use a double-wide CAS here if CPU supports it
                  (progn
                    (setf (%instance-wrapper ,thing) ,layout)
                    (%instance-set ,thing sb-vm:instance-data-start ,slot-vector)))))

(defun %obsolete-instance-trap (owrapper nwrapper instance)
  (cond
    ((layout-for-pcl-obj-p owrapper)
     (binding* ((class (wrapper-class nwrapper))
                (oslots (get-slots instance))
                (nwrapper (class-wrapper class))
                (nslots (make-array (wrapper-length nwrapper)
                                    :initial-element +slot-unbound+))
                (added ())
                (discarded ())
                (plist ())
                (safe (safe-p class))
                ((new-instance-slots nil new-custom-slots)
                 (classify-slotds (wrapper-slot-list nwrapper)))
                ((old-instance-slots old-class-slots old-custom-slots)
                 (classify-slotds (wrapper-slot-list owrapper)))
                (layout (mapcar (lambda (slotd)
                                  ;; Get the names only once.
                                  (cons (slot-definition-name slotd) slotd))
                                new-instance-slots)))
       ;; local  --> local     transfer value, check type
       ;; local  --> shared    discard value, discard slot
       ;; local  -->  --       discard slot
       ;; local  --> custom    XXX

       ;; shared --> local     transfer value, check type
       ;; shared --> shared    -- (cf SHARED-INITIALIZE :AFTER STD-CLASS)
       ;; shared -->  --       discard value
       ;; shared --> custom    XXX

       ;;  --    --> local     add slot
       ;;  --    --> shared    --
       ;;  --    --> custom    XXX
       (flet ((set-value (value cell)
                (%set-slot-value-checking-type
                 "updating obsolete instance"
                 nslots (cdr cell) value safe class class)
                ;; Prune from the list now that it's been dealt with.
                (setf layout (remove cell layout))))

         ;; Go through all the old local slots.
         (dolist (old old-instance-slots)
           (let* ((name (slot-definition-name old))
                  (cell (find-slot-cell owrapper name))
                  (location (car cell))
                  (value (cond
                           ((fixnump location)
                            (clos-slots-ref oslots location))
                           ((not location)
                            (let ((location (slot-info-location (cdr cell))))
                              (aver (integerp location))
                              (clos-slots-ref oslots (slot-info-location (cdr cell)))))
                           (t (bug "non-FIXNUM non-NULL location in cell: ~S" cell)))))
             (unless (unbound-marker-p value)
               (let ((new (assq name layout)))
                 (cond (new
                        (set-value value new))
                       (t
                        (push name discarded)
                        (setf (getf plist name) value)))))))

         ;; Go through all the old shared slots.
         (dolist (old old-class-slots)
           (binding* ((cell (slot-definition-location old))
                      (name (car cell))
                      (new (assq name layout) :exit-if-null))
             (set-value (cdr cell) new)))

         ;; Go through all custom slots to find added ones. CLHS
         ;; doesn't specify what to do about them, and neither does
         ;; AMOP. We do want them to get initialized, though, so we
         ;; list them in ADDED for the benefit of SHARED-INITIALIZE.
         (dolist (new new-custom-slots)
           (let* ((name (slot-definition-name new))
                  (old (find name old-custom-slots
                             :key #'slot-definition-name)))
             (unless old
               (push name added))))

         ;; Go through all the remaining new local slots to compute
         ;; the added slots.
         (dolist (cell layout)
           (push (car cell) added)))

       (replace-wrapper-and-slots instance nwrapper nslots)
       ;; The obsolete instance protocol does not specify what happens if
       ;; an error is signaled in U-I-F-R-C and there is a nonlocal exit
       ;; outside; it may result in a half-updated instance whose
       ;; structure is updated but whose added slots are not initialized.
       ;; (See CLHS 3.7.2.)
       ;; The approach taken here is to abort the update process, as defined
       ;; in CLHS 4.3.6, altogether, and restore the instance to its obsolete
       ;; state; this way the programmer can try to fix the U-I-F-R-C code
       ;; which signaled an error and try to access the instance again
       ;; in order to try and update it again.
       (sb-sys:nlx-protect (update-instance-for-redefined-class
                            instance added discarded plist)
         (replace-wrapper-and-slots instance owrapper oslots))

       nwrapper))
    (*in-obsolete-instance-trap* #.(find-layout 'structure-object))
    (t
     (let ((*in-obsolete-instance-trap* t))
       (error 'obsolete-structure :datum instance)))))

(defun %change-class (copy instance new-class initargs)
  (binding* ((new-wrapper (class-wrapper (ensure-class-finalized new-class)))
             (new-slots (make-array (wrapper-length new-wrapper)
                                    :initial-element +slot-unbound+))
             (old-wrapper (wrapper-of instance))
             (old-class (wrapper-class old-wrapper))
             (old-slots (get-slots instance))
             (safe (safe-p new-class))
             (new-wrapper-slots (wrapper-slot-list new-wrapper)))
    (replace-wrapper-and-slots copy new-wrapper new-slots)
    (flet ((initarg-for-slot-p (slot)
             (when initargs
               (dolist (slot-initarg (slot-definition-initargs slot))
                 (unless (unbound-marker-p
                          (getf initargs slot-initarg +slot-unbound+))
                   (return t)))))
           (set-value (value slotd)
             (%set-slot-value-checking-type
              'change-class new-slots slotd value safe
              old-class new-class)))

      ;; "The values of local slots specified by both the class CTO
      ;; and CFROM are retained. If such a local slot was unbound, it
      ;; remains unbound."
      (dolist (new new-wrapper-slots)
        (when (and (not (initarg-for-slot-p new))
                   (eq (slot-definition-allocation new) :instance))
          (binding* ((cell (find-slot-cell old-wrapper (slot-definition-name new))
                           :exit-if-null)
                     (location (car cell))
                     (value (cond
                              ((fixnump location)
                               (clos-slots-ref old-slots location))
                              ((not location)
                               (let ((info (cdr cell)))
                                 (case (slot-info-allocation info)
                                   (:instance
                                    (clos-slots-ref old-slots (slot-info-location info)))
                                   (:class (cdr (slot-info-location info))))))
                              (t
                               (cdr location)))))
            (set-value value new)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    ;; All uses of %CHANGE-CLASS are under the world lock, but that doesn't
    ;; preclude user code operating on the old slots + new layout or v.v.
    ;; Users need to synchronize their own access when changing class.
    (replace-wrapper-and-slots copy old-wrapper old-slots)
    (replace-wrapper-and-slots instance new-wrapper new-slots)

    ;; The CLHS does not specify what happens if an error is signaled in
    ;; U-I-F-D-C and there is a nonlocal exit outside; it may result in a
    ;; half-updated instance whose class is updated but whose added slots
    ;; are not initialized. (See CLHS 3.7.2.)
    ;; The approach taken here is to abort the change-class process, as
    ;; defined in CLHS 4.3.6, altogether, and restore the instance to its
    ;; previous state; this way the programmer can try to fix the U-I-F-D-C
    ;; code which signaled an error and try to CHANGE-CLASS the instance
    ;; again.
    (sb-sys:nlx-protect (apply #'update-instance-for-different-class
                               copy instance initargs)
      (replace-wrapper-and-slots instance old-wrapper old-slots))

    instance))
)

```
