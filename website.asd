(asdf:defsystem :website
  :build-operation program-op
  :build-pathname "website"
  :entry-point "website:main"
  :serial t
  :depends-on ("hunchentoot"
               "cl-who")
  :components ((:file "main")))
