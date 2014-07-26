(in-package :application-context.example)

(defclass application-base (application-context)
  ())

(defappvar **log-path**)

(defun log-text (text)
  (format t "~A added to ~A~%" text **log-path**))
