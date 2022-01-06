;;;; value-semantics.asd

(asdf:defsystem #:value-semantics-utils
  :description "Metaclasses and utilities for working with value semantics"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:closer-mop #:moptilities)
  :pathname "src"
  :components ((:file "package")
               (:file "eqv")
               (:file "class-with-value-semantics")
               (:file "always-bound-class")
               (:file "typechecked-class")
               (:file "typechecked-class-with-value-semantics"))
  :in-order-to ((test-op (load-op :value-semantics-utils/test)))
  :perform
  (test-op (o c)
           (symbol-call
            '#:parachute '#:test
            (find-symbol (symbol-name '#:value-semantics-utils)
                         (find-package '#:value-semantics-utils/test))
            :report (find-symbol "INTERACTIVE"
                                 "PARACHUTE"))))

(asdf:defsystem #:value-semantics-utils/test
  :description "Tests for value-semantics-utils"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:parachute)
  :pathname "t"
  :components ((:file "package")
               (:file "eqv")
               (:file "class-with-value-semantics")
               (:file "always-bound-class")
               (:file "typechecked-class")
               (:file "typechecked-class-with-value-semantics")))
