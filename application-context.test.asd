(asdf:defsystem :application-context.test
  :depends-on (:lift :application-context)
  :components ((:file "test/package")
               (:file "test/test")))
