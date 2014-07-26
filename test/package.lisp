(defpackage :application-context.test
  (:use :cl :application-context)
  (:import-from :lift
                #:deftestsuite
                #:ensure-same
                #:ensure-error
                #:run-tests))
