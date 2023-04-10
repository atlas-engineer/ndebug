;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem #:ndebug
  :description "A toolkit to construct interface-aware yet standard-compliant debugger hooks."
  :author "Atlas Engineer LLC"
  :license  "BSD 3-Clause"
  :version "0.2.0"
  :serial t
  :depends-on (#:dissect #:trivial-custom-debugger #:bordeaux-threads #:trivial-gray-streams)
  :components ((:file "package")
               (:file "stream")
               (:file "ndebug"))
  :in-order-to ((test-op (test-op "ndebug/tests")
                         (test-op "ndebug/tests/compilation"))))

(defsystem "ndebug/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "ndebug/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on ("ndebug")
  :targets (:package :ndebug/tests)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")))

(defsystem "ndebug/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :undocumented-symbols-to-ignore (:restarts :condition-itself :stack)
  :depends-on ("ndebug")
  :packages (:ndebug))
