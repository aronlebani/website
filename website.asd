(in-package :asdf)

(defsystem :website
  :build-operation program-op
  :build-pathname "website"
  :entry-point "website:main"
  :serial t
  :depends-on ("djula"
               "hunchentoot"
               "cl-markdown")
  :components ((:file "main")))
