;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:ndebug)

(defvar *query-write* nil
  "A function/lambda to unconditionally override/alter the `query-write' call.")
(defvar *query-read* nil
  "A function/lambda to unconditionally override/alter the `query-read' call.")
(defvar *ui-display* nil
  "A function/lambda to unconditionally override/alter the `ui-display' call.")
(defvar *ui-cleanup* nil
  "A function/lambda to unconditionally override/alter the `ui-cleanup' call.")

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
   (chosen-restart
    :initform nil
    :documentation "The restart chosen in the interface and brought by `invoke'.")
   (code-to-evaluate
    :initform nil
    :type (or list function)
    :documentation "The code to evaluate in `evaluate'.
Can be either a list of a zero-argument function.")
   (restart-semaphore
    :initform (bt:make-semaphore)
    :type bt:semaphore
    :documentation "The semaphore to wait on until the restart is returned.")
   (stack
    :initform nil
    :initarg :stack
    :accessor stack
    :documentation "The state of call stack at the time of the condition firing.
A list of `dissect:call' objects."))
  (:documentation "The wrapper for condition.

Made so that `*debugger-hook*' can wait for the condition to be resolved based on
the `channel', wrapped alongside the condition and its restarts."))

(defgeneric query-write (wrapper string)
  (:method ((wrapper condition-wrapper) (string string))
    nil)
  (:method :around (wrapper string)
    (if *query-write*
        (funcall *query-write* wrapper string)
        (call-next-method)))
  (:documentation "The function to call as part of custom `*query-io*' when prompting the user.
Always prefers `*query-write*' (if set) over the default method."))

(defgeneric query-read (wrapper)
  (:method ((wrapper condition-wrapper))
    nil)
  (:method :around (wrapper)
    (if *query-read*
        (funcall *query-read* wrapper)
        (call-next-method)))
  (:documentation "The function to call as part of custom `*query-io*' when getting user input.
Always prefers `*query-read*' (if set) over the default method."))

(defgeneric ui-display (wrapper)
  (:method ((wrapper condition-wrapper))
    nil)
  (:method :around (wrapper)
    (if *ui-display*
        (funcall *ui-display* wrapper)
        (call-next-method)))
  (:documentation "Part of custom debugger, called when showing the condition to the user.
Always prefers `*ui-display*' (if set) over the default method."))

(defgeneric ui-cleanup (wrapper)
  (:method ((wrapper condition-wrapper)))
  (:method :around (wrapper)
    (if *ui-cleanup*
        (funcall *ui-cleanup* wrapper)
        (call-next-method)))
  (:documentation "Part of custom debugger, called once the debugger is done.
Always prefers `*ui-cleanup*' (if set) over the default method."))

(declaim (ftype (function (&key (:wrapper-class t)
                                (:ui-display (or null (function (condition-wrapper))))
                                (:ui-cleanup (or null (function (condition-wrapper))))
                                (:query-read (or null (function (condition-wrapper) string)))
                                (:query-write (or null (function (condition-wrapper string))))))
                make-debugger-hook))
(defun make-debugger-hook (&key (wrapper-class 'condition-wrapper)
                             (ui-display *ui-display*) (ui-cleanup *ui-cleanup*)
                             (query-read *query-read*) (query-write *query-write*))
  "Construct a `*debugger-hook*'-compatible function with multi-threading and UI interaction.

WRAPPER-CLASS is a class designator for the class to wrap the
condition in. Defaults to `condition-wrapper'. WRAPPER-CLASS
designated class must inherit from `condition-wrapper'.

UI-DISPLAY is a function to invoke when showing the debugger
window/prompt/query. Is called with a condition wrapper to
display. Overrides a `ui-display' method (if present), defined for the
WRAPPER-CLASS.

UI-CLEANUP is a function to invoke after the debugging is done and the
interface is in need of cleaning up (like removing debug windows or
flushing the shell.) Accepts a condition wrapper to clean up
after. Overrides a `ui-cleanup' method (if present), defined for the
WRAPPER-CLASS.

QUERY-READ is a function to invoke when querying the user, like
opening a an input window or waiting for shell input. Must return an
inputted string. The only argument is the condition wrapper for a
related condition. Overrides a `query-read' method (if present),
defined for the WRAPPER-CLASS.

QUERY-WRITE is a unary function to invoke when showing the user the
prompting text, like when opening a dialogue window or writing to the
shell. Can refer to the outside state to interface with the
QUERY-READ. The arguments are:
- Condition wrapper for the current condition.
- The string to show to the user.
Overrides a `query-write' method (if present), defined for the
WRAPPER-CLASS.

QUERY-READ and QUERY-WRITE should both be present (in which case
prompting happens in the custom interface), or both absent (in which
case the default `*query-io*' is used.)"
  (lambda (condition hook)
    (let* ((restarts (dissect:restarts))
           (wrapper (make-instance wrapper-class
                                   :condition-itself condition
                                   :restarts restarts
                                   :stack (dissect:stack)))
           (*query-io* (if (or (and (ignore-errors (find-method #'query-read nil (list wrapper-class)))
                                    (ignore-errors (find-method #'query-write nil (list wrapper-class 'string))))
                               (and query-read query-write))
                           (make-debugger-stream
                            (lambda ()
                              (let* ((*query-read* query-read)
                                     (*debugger-hook* nil)
                                     (result (query-read wrapper)))
                                (if (uiop:string-suffix-p result #\newline)
                                    result
                                    (uiop:strcat result #\newline))))
                            (lambda (string)
                              (let ((*query-write* query-write)
                                    (*debugger-hook* nil))
                                (query-write wrapper string))))
                           *query-io*)))
      (when (or (ignore-errors (find-method #'ui-display nil (list wrapper-class)))
                ui-display)
        (let ((*ui-display* ui-display)
              (*debugger-hook* nil))
          (ui-display wrapper)))
      (unwind-protect
           ;; FIXME: Waits indefinitely. Should it?
           (let ((restart (loop for got-something = (bt:wait-on-semaphore (slot-value wrapper 'restart-semaphore))
                                for code = (slot-value wrapper 'code-to-evaluate)
                                for restart = (slot-value wrapper 'chosen-restart)
                                when code
                                  do (typecase code
                                       (list (eval code))
                                       (function (funcall code)))
                                  and do (setf (slot-value wrapper 'code-to-evaluate) nil)
                                else when restart
                                       do (return restart)))
                 (*debugger-hook* hook))
             (invoke-restart-interactively
              (etypecase restart
                (dissect:restart (dissect:object restart))
                (restart restart)
                (symbol (find-restart restart))
                (function restart))))
        (when (or (ignore-errors (find-method #'ui-cleanup nil (list wrapper-class)))
                  ui-cleanup)
          (let ((*ui-cleanup* ui-cleanup)
                (*debugger-hook* nil))
            (ui-cleanup wrapper)))))))

(defgeneric invoke (wrapper restart)
  (:method ((wrapper condition-wrapper) (restart symbol))
    (setf (slot-value wrapper 'chosen-restart) restart)
    (bt:signal-semaphore (slot-value wrapper 'restart-semaphore)))
  (:method ((wrapper condition-wrapper) (restart dissect:restart))
    (setf (slot-value wrapper 'chosen-restart) restart)
    (bt:signal-semaphore (slot-value wrapper 'restart-semaphore)))
  (:method ((wrapper condition-wrapper) (restart restart))
    (setf (slot-value wrapper 'chosen-restart) restart)
    (bt:signal-semaphore (slot-value wrapper 'restart-semaphore)))
  (:method ((wrapper condition-wrapper) (restart function))
    (setf (slot-value wrapper 'chosen-restart) restart)
    (bt:signal-semaphore (slot-value wrapper 'restart-semaphore)))
  (:documentation "Invoke the RESTART in the initial debugger hook of the WRAPPER.

The RESTART should be one of the `restarts' of the WRAPPER. Otherwise
the behavior is implementation-dependent, but never exactly pretty."))

(defgeneric evaluate (wrapper code)
  (:method ((wrapper condition-wrapper) (code list))
    (setf (slot-value wrapper 'code-to-evaluate) code)
    (bt:signal-semaphore (slot-value wrapper 'restart-semaphore)))
  (:method ((wrapper condition-wrapper) (code function))
    (setf (slot-value wrapper 'code-to-evaluate) code)
    (bt:signal-semaphore (slot-value wrapper 'restart-semaphore)))
  (:documentation "Evaluate the CODE in the debugger WRAPPER context.

CODE can be
- A quoted list of Lisp code, in which case it will be avaluated.
- A function object, in which case if will be called in the context of the debugger."))

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
               (list :wrapper-class wrapper-class))
           ,@(when query-read
               (cons :query-read (wrap-lambda-maybe query-read)))
           ,@(when query-write
               (list :query-write
                     (if (member (first query-write) '(lambda function))
                         query-write
                         `(lambda (,(alexandria:symbolicate "%WRAPPER%")
                                   ,(alexandria:symbolicate "%STRING%"))
                            (declare (ignorable ,(alexandria:symbolicate "%WRAPPER%")
                                                ,(alexandria:symbolicate "%STRING%")))
                            ,query-write))))
           ,@(when ui-display
               (cons :ui-display (wrap-lambda-maybe ui-display)))
           ,@(when ui-cleanup
               (cons :ui-cleanup (wrap-lambda-maybe ui-cleanup)))))
       ,@body)))
