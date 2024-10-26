(in-package :asdf)

(defsystem :website
  :build-operation program-op
  :build-pathname "website"
  :entry-point "website:main"
  :serial t
  :depends-on ("eco"
               "hunchentoot"
               "cl-markdown")
  :components ((:file "package")
               (:file "main")))
