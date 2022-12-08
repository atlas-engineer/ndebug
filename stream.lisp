;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:ndebug)

(defclass debugger-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((input-fn :initarg :input-fn
             :accessor input-fn
             :documentation "The one-shot function returning string.
This string is then used for all the input operations.")
   (input :initarg :input
          :accessor input
          :documentation "The string to use as input buffer.")
   (index :initform 0
          :initarg :index
          :accessor index
          :documentation "The index in the string.")))

(defmethod slot-unbound (class (stream debugger-input-stream) (slot-name (eql 'input)))
  (setf (input stream) (funcall (input-fn stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream debugger-input-stream))
  (when (= (index stream) (length (input stream)))
    (setf (input stream) (funcall (input-fn stream))
          (index stream) 0)
    (when (uiop:emptyp (input stream))
      (return-from trivial-gray-streams:stream-read-char :eof)))
  (prog1
      (char (input stream) (index stream))
    (incf (index stream))))

(defmethod trivial-gray-streams:stream-listen ((stream debugger-input-stream))
  (< (index stream) (length (input stream))))

(defmethod trivial-gray-streams:stream-unread-char ((stream debugger-input-stream) char)
  (decf (index stream))
  nil)

(defmethod trivial-gray-streams:stream-clear-input ((stream debugger-input-stream))
  (setf (input stream) ""
        (index stream) 0)
  nil)

(defmethod trivial-gray-streams:stream-line-column ((stream debugger-input-stream))
  nil)

(defclass debugger-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output-fn :initarg :output-fn
              :accessor output-fn
              :documentation "The one-shot function accepting string and printing it.")
   (output :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)
           :initarg :output
           :accessor output
           :documentation "The vector to use as the output buffer.")
   (column :initform 0
           :initarg :column
           :accessor column)))

(defmethod trivial-gray-streams:stream-write-char ((stream debugger-output-stream) char)
  (vector-push-extend char (output stream))
  (if (char= #\newline char)
      (setf (column stream) 0)
      (incf (column stream)))
  char)

(defmethod trivial-gray-streams:stream-line-column ((stream debugger-output-stream))
  (column stream))

(defmethod trivial-gray-streams:stream-finish-output ((stream debugger-output-stream))
  (funcall (output-fn stream) (coerce (output stream) 'string))
  (loop until (zerop (length (output stream)))
        do (vector-pop (output stream)))
  nil)

(defmethod trivial-gray-streams:stream-force-output ((stream debugger-output-stream))
  (trivial-gray-streams:stream-finish-output stream))

(defmethod trivial-gray-streams:stream-fresh-line ((stream debugger-output-stream))
  (cond ((zerop (column stream)) nil)
        (t (terpri stream) t)))


(declaim (ftype (function ((function () string) (function (string))) two-way-stream)
                make-debugger-stream))
(defun make-debugger-stream (input-fn output-fn)
  "Construct a `*query-io*'-compatible stream out of INPUT-FN and OUTPUT-FN."
  (make-two-way-stream
   ;; FIXME: Understand/reproduce how Swank makes those streams.
   (make-instance 'debugger-input-stream :input-fn input-fn)
   (make-instance 'debugger-output-stream :output-fn output-fn)))
