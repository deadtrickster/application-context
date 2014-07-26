(in-package :application-context)

(define-condition unknown-application (error)
  ()
  (:documentation "Signaled when one tries to get/set appvar value without application context"))

(defclass application-context ()
  (#-single-insance
   (storage :reader application-context-storage :initform (make-hash-table))))

(defmacro defglobalvar (var value &optional doc)
  #+sbcl `(sb-ext:defglobal ,var ,value ,doc)
  #-sbcl `(defvar ,var ,value ,doc))

#-single-instance
(defvar *application* nil
  "Current application instance")
#+single-instance
(defglobalvar *application*
  "Global application instance")

(unless (boundp '+default+)
  (defconstant +default+ (gensym)
    "Value used as default for storage lookups"))

#+single-instance
(defglobalvar *var-initializers* ())

(defun appvar (name)
  (assert *application* () (make-instance 'unknown-application))
  (let ((value (gethash name (application-context-storage *application*) +default+)))
    (if (eq value +default+)
        (setf (gethash name (application-context-storage *application*)) (funcall (get name 'default)))
        value)))

(defun (setf appvar) (new-value name)
  (assert *application* () (make-instance 'unknown-application))
  (setf (gethash name (application-context-storage *application*)) new-value))

(defmacro defappvar (name &optional (value nil) documentation)
  "Defines variable whose value depends on application context"
  #-single-instance
  `(progn
     (setf (get ',name 'default) (lambda () ,value))
     (define-symbol-macro ,name
         (appvar ',name))
     (when ,documentation (setf  (documentation ',name 'variable) ,documentation)))
  #+single-instance
  `(progn
     (defglobalvar ,name +default+)
     (push (lambda ()
             (setq ,name ,value))
           *var-initializers*)
     (when ,documentation (setf  (documentation ',name variable) ,documentation))))

#+single-instance
(defun init-variables ()
  (let ((initializers (reverse *var-initializers*)))
    (loop for initializer in initializers do
             (funcall initializer))))

#+single-instance
(defmethod initialize-instance :after ((instance application-context) &rest args)
  (declare (ignorable args))
  (init-variables)
  (when (next-method-p)
    (call-next-method)))

#-single-instance
(defmacro with-application (app-var &body body)
  "Sets application context for context-dependent variables"
  `(let ((*application* ,app-var))
     ,@body))

#+single-instance
(defmacro with-application (app-var &body body)
  (declare (ignore app-var))
  `(progn ,@body))
