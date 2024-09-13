;;;; main.lisp
;;;;
;;;; Website for https://lebani.dev
;;;;
;;;; Aron Lebani <aron@lebani.dev>
 
(defpackage :website
  (:use #:cl)
  (:import-from #:hunchentoot
                #:create-prefix-dispatcher
                #:create-folder-dispatcher-and-handler
                #:easy-acceptor
                #:start
                #:stop
                #:return-code*
                #:*dispatch-table*)
  (:import-from #:djula
                #:add-template-directory
                #:compile-template*
                #:render-template*
                #:*default-template-arguments*)
  (:import-from #:cl-markdown
                #:markdown)
  (:import-from #:bt
                #:join-thread
                #:all-threads
                #:thread-name)
  (:import-from #:uiop
                #:directory-files
                #:quit)
  (:import-from #:asdf
                #:system-relative-pathname)
  (:export #:main))

(in-package #:website)

;;; --- Helpers ---

(defmacro defroute ((name path) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher ,path #',name)
           *dispatch-table*)))

(defmacro defstatic (uri path)
  `(push (create-folder-dispatcher-and-handler ,uri ,path)
         *dispatch-table*))

(defun get-year ()
  (nth 5
       (multiple-value-list (decode-universal-time (get-universal-time)))))

(defun push-template (template-path)
  (let ((name (file-namestring template-path)))
    (setf (gethash name *templates*)
          (compile-template* name))))

(defun register-templates (template-directory)
  (defparameter *templates*
    (make-hash-table :test #'equal))

  (add-template-directory (system-relative-pathname "website" "templates/"))

  (mapc #'push-template
        (directory-files template-directory)))

(defmacro render (template-name &rest objects)
  `(render-template* (gethash ,template-name *templates*)
                     nil
                     ,@objects))

;;; --- Build articles ---

; Build articles at compile time so it doesn't have to do all this work on
; each page render.
(eval-when (:compile-toplevel)
  (defun markdown->article (markdown-path)
    (let ((date (pathname-name markdown-path))
          (content (with-output-to-string (s)
                     (markdown markdown-path :stream s))))
      `(:date ,date :content ,content)))

  (defun by-date (a b)
    (string< (getf b :date)
             (getf a :date)))

  (defun compile-articles ()
    (sort (mapcar #'markdown->article
                  (directory-files "articles/"))
          #'by-date))

  (defparameter *articles* (compile-articles)))

;;; --- Define pages ---

(defroute (index "/")
  (render "index.html"))

(defroute (projects "/projects")
  (render "projects.html"))

(defroute (now "/now")
  (render "now.html" :articles *articles*))

(defroute (reading "/reading")
  (render "reading.html"))

(defroute (quicklinks "/quicklinks")
  (render "quicklinks.html"))

(defroute (make-coffee "/make-coffee")
  (setf (return-code*) 418)
  (render "418.html"))

;;; --- Serve static files ---

(defstatic "/public/" #p"public/")

;;; --- Entrypoint ---

(defparameter *port*
  4000)

(defparameter *server*
  (make-instance 'easy-acceptor :port *port*))

(defun main ()
  (format t "Hunchentoot server is started~&")
  (format t "Listenting on localhost:~a~&" *port*)

  (register-templates "templates/")
  (setf (getf *default-template-arguments* :year) (get-year))

  (start *server*)

  (handler-case (join-thread
                  (find-if (lambda (th)
                             (search "hunchentoot" (thread-name th)))
                           (all-threads)))
    ; Catch C-c
    (sb-sys:interactive-interrupt ()
      (progn
        (format *error-output* "Aborting~&")
        (stop *server*)
      (quit)))
    ; Something went wrong
    (error (c)
      (format t "An unknown error occured:~&~a~&" c))))
