;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:ndebug
  (:use #:cl)
  (:export
   #:condition-wrapper
   #:condition-itself
   #:restarts
   #:channel
   #:stack
   #:make-debugger-stream
   #:make-debugger-hook
   #:with-debugger-hook)
  (:documentation "NDebug provides several primitives to work with UI-aware debuggers:

`ndebug:condition-wrapper' as a class to encapsulate all the
meta-information about the condition, otherwise only available in the
debugger hook. With this class, NDebug can pass condition to be
handled elsewhere, including the graphical debugger. Important slots:
- `ndebug:condition-itself' as a condition debugger got.
- `ndebug:restarts' as a list of CL restarts connected to the
  condition.
- `ndebug:stack' as a list of `dissect:call's representing the call
  stack state at the moment of condition signalling.
- `ndebug:channel' as an internal channel to pass the chosen restart
  through.

`ndebug:make-debugger-stream' constructs a `*query-io*'-friendly
stream based on the input and output functions passed to it. For now,
it's a thin wrapper around the `swank-backend:make-input-stream' and
`swank-backend:make-output-stream', but that may change in the future.

`ndebug:make-debugger-hook' constructs the UI-aware debugger so that
thing you have to provide is a set of functions to:
- Query the user (:QUERY-READ).
- Show the user debugger prompt (:QUERY-WRITE).
- Show the condition in the UI (:UI-DISPLAY).
- Clean the UI after the condition is handled (:UI-DISPLAY).

Additionally `ndebug:make-debugger-hook' accepts a :WRAPPER-CLASS so
that you can provide your own wrapper class instead of
`ndebug:condition-wrapper'. Note that it has to inherit from
`ndebug:condition-wrapper' for all the NDebug APIs to work properly.

`ndebug:with-debugger-hook' is a thin wrapper around
`ndebug:make-debugger-hook' to bind the debugger hook to the generated
function for the duration of the body."))
