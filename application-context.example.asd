(asdf:defsystem :application-context.example
  :depends-on (:lift :application-context)
  :components ((:file "example/package")
               (:file "example/application-base")
               (:file "example/application1")
               (:file "example/application2")
               (:file "example/run")))
