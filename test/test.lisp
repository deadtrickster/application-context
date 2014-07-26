(in-package :application-context.test)

(defmacro addtest ((test-name suite-name &rest options) &body body)
  (let* ((documentation (if (stringp (car body)) (list :documentation (car body))))
         (body (if documentation (rest body) body))
         (addtest-args (append (list suite-name) options documentation)))
    `(lift:addtest ,addtest-args
       ,test-name
       ,@body)))

(defclass application1 (application-context) ())
(defvar *application1* (make-instance 'application1))

(defclass application2 (application-context) ())
(defvar *application2* (make-instance 'application2))

(defappvar test-var) ;;default value is nil

;;default value here depends on value of test-var
;;it is evaluated only once in current application context
(defappvar test-var-with-default (1+ test-var))

(deftestsuite application-context-tests ()
  ()
  (:setup (progn
            (setf (slot-value *application1* 'application-context::storage) (make-hash-table)
                  (slot-value *application2* 'application-context::storage) (make-hash-table)))))

(addtest (test-default-to-nil application-context-tests)
  (with-application *application1*
    (ensure-same test-var nil))
  (with-application *application2*
    (ensure-same test-var nil)))

(defun return-test-var ()
  test-var)

(addtest (test-dynamic-context application-context-tests)
  (with-application *application1*
    (setf test-var 1)
    (ensure-same (return-test-var) 1 :test #'eql))
  (with-application *application2*
    (setf test-var 2)
    (ensure-same (return-test-var) 2 :test #'eql)))


(addtest (test-computed-default-value application-context-tests)
  (with-application *application1*
    (setf test-var 1)
    (ensure-same test-var-with-default 2 :test #'eql)
    (setf test-var 2)
    (ensure-same test-var-with-default 2 :test #'eql))) ;;default value computed only once

(addtest (test-error-without-application-context application-context-tests)
  (ensure-error test-var))

(addtest (test-defappvar-is-not-defvar application-context-tests)
  (with-application *application1*
    (setf test-var 1)
    (let ((test-var 6))
      (ensure-same (return-test-var) 1 :test #'eql))))

;; when signle-instance is in *features* defappvar is sb-ext:defglobal

;; (push :single-instance *features*)

;; (defappvar single-instance-var)

;; (addtest test-dynamic-context application-context-tests
;;   (with-application *application1*
;;     (setf single-instance-var 1)
;;     (ensure-same single-instance-var 1 :test #'eql))
;;   (with-application *application2*
;;     (ensure-same single-instance-var 1 :test #'eql))) ;; note how single-instane-var here evaluates to 1 instead of nil


;; (pop :single-instance *features*)


(describe (run-tests :suite 'application-context-tests))
