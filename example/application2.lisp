(in-package :application-context.example)

(defclass application2 (application-base)
  ())

(defmethod initialize-instance :after ((instance application2) &rest args)
  (declare (ignore args))
  (with-application instance
    (setf **log-path** "/tmp/application2.log")))
