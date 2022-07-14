;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem #:ndebug
  :description "A toolkit to construct interface-aware yet standard-compliant debugger hooks."
  :author "Atlas Engineer LLC"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:dissect #:trivial-custom-debugger #:lparallel #:swank)
  :components ((:file "package")
               (:file "ndebug")))
