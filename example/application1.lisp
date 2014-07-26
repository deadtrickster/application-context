(in-package :application-context.example)

(defclass application1 (application-base)
  ())

(defmethod initialize-instance :after ((instance application1) &rest args)
  (declare (ignore args))
  (with-application instance
    (setf **log-path** "/tmp/application1.log")))
