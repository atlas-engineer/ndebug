;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:ndebug/tests)

(setf lparallel:*kernel* (lparallel:make-kernel 1))

(defvar what "hello")

(defun error-with-ignore ()
  (restart-case
      (1+ what)
    (ignore () "hello1")
    (ignore2 () "hello2")))

(defun find-restart-by-name (name restarts)
  (loop for r in restarts
        when (search name
                     (symbol-name
                      (typecase r
                        (restart (restart-name r))
                        (dissect:restart (restart-name (dissect:object r)))))
                     :test #'string-equal)
          do (return r)))

(define-test debugger-handler-bind ()
  (let ((*debugger-hook*
          (ndebug:make-debugger-hook
           :ui-display (lambda (wrapper)
                         (ndebug:invoke
                          wrapper
                          (assert-true (find-restart-by-name "supersede" (ndebug:restarts wrapper))))))))
    (uiop:with-temporary-file (:pathname p)
      (assert-true (uiop:file-exists-p p))
      (assert-equal "" (uiop:read-file-string p))
      (let ((s (open p :direction :output)))
        (assert-true (uiop:file-exists-p p))
        (format s "hello")
        (force-output s)
        (assert-equal "hello" (uiop:read-file-string p))
        (close s))
      (assert-true (uiop:file-exists-p p)))))

(define-test multithreaded ()
  (let ((*debugger-hook*
          (ndebug:make-debugger-hook
           :ui-display (lambda (wrapper)
                         (bt:make-thread
                          (lambda ()
                            (ndebug:invoke
                             wrapper
                             (find-restart-by-name "ignore" (ndebug:restarts wrapper)))))))))
    (assert-equal "hello1" (error-with-ignore))))

(define-test with-debugger-hook-expansion ()
  (ndebug:with-debugger-hook
      (:ui-display (ndebug:invoke
                    %wrapper%
                    (assert-true (find-restart-by-name "ignore" (ndebug:restarts %wrapper%)))))
    (assert-equal "hello1" (error-with-ignore))))

(defclass my-wrapper (ndebug:condition-wrapper)
  ((restart-findable-name :initform "ignore"
                          :accessor restart-findable-name
                          :type string)))

(defmethod ndebug:ui-display ((wrapper my-wrapper))
  (ndebug:invoke wrapper (find-restart-by-name (restart-findable-name wrapper)
                                               (ndebug:restarts wrapper))))

(define-test class-based-debugging ()
  (ndebug:with-debugger-hook
      (:wrapper-class 'my-wrapper)
    (assert-equal "hello1" (error-with-ignore))))

(define-test class-based-debugging-overriden ()
  (ndebug:with-debugger-hook
      (:wrapper-class 'my-wrapper
       :ui-display (ndebug:invoke
                    %wrapper%
                    (assert-true (find-restart-by-name "ignore2" (ndebug:restarts %wrapper%)))))
    (assert-equal "hello2" (error-with-ignore))))
