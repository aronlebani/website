(asdf:defsystem :website
  :build-operation program-op
  :build-pathname "server"
  :entry-point "website:main"
  :serial t
  :depends-on ("hunchentoot"
               "cl-who")
  :components ((:file "main")))
