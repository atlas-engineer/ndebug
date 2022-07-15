;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:ndebug)

(defclass condition-wrapper ()
  ((condition-itself
    :initform (error "condition-wrapper should always wrap a condition.")
    :initarg :condition-itself
    :accessor condition-itself
    :type condition
    :documentation "The condition itself.")
   (restarts
    :initform '()
    :initarg :restarts
    :accessor restarts
    :type list
    :documentation "A list of `dissect:restart's for the given condition.")
   (channel
    :initform nil
    :initarg :channel
    :type (or null lparallel:channel)
    :documentation "The channel to signal the chosen restart through.")
   (stack
    :initform nil
    :initarg :stack
    :accessor stack
    :documentation "The state of call stack at the time of the condition firing.
A list of `dissect:call' objects."))
  (:documentation "The wrapper for condition.

Made so that `*debugger-hook*' can wait for the condition to be resolved based on
the `channel', wrapped alongside the condition and its restarts."))

(declaim (ftype (function ((function () string) (function (string))) two-way-stream)
                make-debugger-stream))
(defun make-debugger-stream (input-fn output-fn)
  "Construct a `*query-io*'-compatible stream out of INPUT-FN and OUTPUT-FN."
  (make-two-way-stream
   ;; FIXME: Understand/reproduce how Swank makes those streams.
   (swank-backend:make-input-stream input-fn)
   (swank-backend:make-output-stream output-fn)))

(declaim (ftype (function (&key (:wrapper-class t)
                                (:query-read (or null (function (condition-wrapper) string)))
                                (:query-write (or null (function (condition-wrapper string))))
                                (:ui-display (or null (function (condition-wrapper))))
                                (:ui-cleanup (or null (function (condition-wrapper))))))
                make-debugger-hook))
(defun make-debugger-hook (&key (wrapper-class 'condition-wrapper)
                             query-read query-write ui-display ui-cleanup)
  "Construct a `*debugger-hook*'-compatible function with multi-threading and UI interaction.

WRAPPER-CLASS is a class designator for the class to wrap the
condition in. Defaults to `condition-wrapper'. WRAPPER-CLASS
designated class must inherit from `condition-wrapper'.

UI-DISPLAY is a function to invoke when showing the debugger
window/prompt/query. Is called with a condition wrapper to display.

UI-CLEANUP is a function to invoke after the debugging is done and the
interface is in need of cleaning up (like removing debug windows or
flushing the shell.) Accepts a condition wrapper to clean up after.

QUERY-READ is a function to invoke when querying the user, like
opening a an input window or waiting for shell input. Must return an
inputted string. The only argument is the condition wrapper for a
related condition.

QUERY-WRITE is a unary function to invoke when showing the user the
prompting text, like when opening a dialogue window or writing to the
shell. Can refer to the outside state to interface with the
QUERY-READ. The arguments are:
- Condition wrapper for the current condition.
- The string to show to the user.

QUERY-READ and QUERY-WRITE should both be present (in which case
prompting happens in the custom interface), or both absent (in which
case the default `*query-io*' is used.)"
  (lambda (condition hook)
    (let* ((restarts (dissect:restarts))
           (channel (lparallel:make-channel :fixed-capacity 1))
           (wrapper (make-instance wrapper-class
                                   :condition-itself condition
                                   :restarts restarts
                                   :channel channel
                                   :stack (dissect:stack)))
           (*query-io* (if (and query-read query-write)
                           (make-debugger-stream
                            (lambda ()
                              (let ((result (funcall query-read wrapper)))
                                (if (uiop:string-suffix-p result #\newline)
                                    result
                                    (uiop:strcat result #\newline))))
                            (lambda (string)
                              (funcall query-write wrapper string)))
                           *query-io*)))
      (when ui-display
        (funcall ui-display wrapper))
      (unwind-protect
           ;; FIXME: Waits indefinitely. Should it?
           (let* ((*debugger-hook* hook)
                  (restart (lparallel:receive-result channel)))
             (invoke-restart-interactively
              (etypecase restart
                (dissect:restart (dissect:object restart))
                (restart restart)
                (symbol (find-restart restart))
                (function restart))))
        (when ui-cleanup
          (funcall ui-cleanup wrapper))))))

(defgeneric invoke (wrapper restart)
  (:method ((wrapper condition-wrapper) (restart symbol))
    (lparallel:submit-task (slot-value wrapper 'channel) (constantly restart)))
  (:method ((wrapper condition-wrapper) (restart dissect:restart))
    (lparallel:submit-task (slot-value wrapper 'channel) (constantly restart)))
  (:method ((wrapper condition-wrapper) (restart restart))
    (lparallel:submit-task (slot-value wrapper 'channel) (constantly restart)))
  (:method ((wrapper condition-wrapper) (restart function))
    (lparallel:submit-task (slot-value wrapper 'channel) (constantly restart)))
  (:documentation "Invoke the RESTART in the initial debugger hook of the WRAPPER.

The RESTART should be one of the `restarts' of the WRAPPER. Otherwise
the behavior is implementation-dependent, but never exactly pretty."))

(defmacro with-debugger-hook ((&key wrapper-class query-read query-write ui-display ui-cleanup)
                              &body body)
  "Execute the BODY with the newly-created (as per `make-debugger-hook') debugger hook.

The ARGS are `make-debugger-hook' arguments passed to it with the
following rules:
- If the argument form starts with a `lambda' or `function' (which
  sharp-quote expands to), pass it to `make-debugger-hook' as-is.
- If not, then wrap it in a lambda with a special variable
  %WRAPPER% (and %STRING% in case of :QUERY-WRITE) accessible to the
  argument form.

Example:"
  (declare (ignorable wrapper-class query-read query-write ui-display ui-cleanup))
  (flet ((wrap-lambda-maybe (form)
           (list (if (member (first form) '(lambda function))
                     form
                     `(lambda (,(alexandria:symbolicate "%WRAPPER%"))
                        (declare (ignorable ,(alexandria:symbolicate "%WRAPPER%")))
                        ,form)))))
    `(trivial-custom-debugger:with-debugger
         ((make-debugger-hook
           ,@(when wrapper-class
               (list wrapper-class))
           ,@(when query-read
               (wrap-lambda-maybe query-read))
           ,@(when query-write
               (list (if (member (first query-write) '(lambda function))
                         query-write
                         `(lambda (,(alexandria:symbolicate "%WRAPPER%")
                                   ,(alexandria:symbolicate "%STRING%"))
                            (declare (ignorable ,(alexandria:symbolicate "%WRAPPER%")
                                                ,(alexandria:symbolicate "%STRING%")))
                            ,query-write))))
           ,@(when ui-display
               (wrap-lambda-maybe ui-display))
           ,@(when ui-cleanup
               (wrap-lambda-maybe ui-cleanup))))
       ,@body)))
