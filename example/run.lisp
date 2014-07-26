(in-package :application-context.example)

(defun run ()
  (with-application (make-instance 'application1)
    (log-text "Application1 message"))
  (with-application (make-instance 'application2)
    (log-text "Application2 message")))
