;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem #:ndebug
  :description "A toolkit to construct interface-aware yet standard-compliant debugger hooks."
  :author "Atlas Engineer LLC"
  :license  "BSD 3-Clause"
  :version "0.1.0"
  :serial t
  :depends-on (#:dissect #:trivial-custom-debugger #:bordeaux-threads #:swank)
  :components ((:file "package")
               (:file "ndebug"))
  :in-order-to ((test-op (test-op "ndebug/tests"))))

(defsystem "ndebug/tests"
  :depends-on (ndebug lisp-unit2)
  :serial t
  :components ((:file "tests/package")
               (:file "tests/tests"))
  :perform (test-op (o c)
                    (symbol-call :lisp-unit2 :run-tests
                                 :package :ndebug/tests
                                 :run-contexts (symbol-function
                                                (read-from-string "lisp-unit2:with-summary-context")))))
