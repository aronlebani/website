;;;; main.lisp
;;;;
;;;; Website for https://lebani.dev
;;;;
;;;; Aron Lebani <aron@lebani.dev>
 
(in-package #:website)

;;; --- Helpers ---

(defun get-year ()
  (nth 5
       (multiple-value-list (decode-universal-time (get-universal-time)))))

(defun walk-directory (base-directory fn)
  (progn
    (mapc (lambda (path)
            (funcall fn path))
          (directory-files base-directory))
    (mapc (lambda (path)
            (walk-directory path fn))
          (subdirectories base-directory))))

(defparameter *template-table*
  (make-hash-table :test :equal))

(defparameter *layout-table*
  (make-hash-table :test :equal))

(defun compile-pages ()
  (flet compile-templates (dir table)
    (walk-directory dir
                    (lambda (file)
                      (let ((file-contents (read-file-string file)))
                        (setf (gethash file table)
                              (compile-template file-contents)))))
    (compile-templates *content-directory*
                       *template-table*)
    (compile-templates *layout-directory*
                       *layout-table*)))

(defun render (template &key layout &optional objects)
  (let ((template-func (gethash template *template-table*)))
    (let ((template-string (funcall template-func objects)))
      (if layout
          (let ((layout-func (gethash layout *layout-table*)))
            (funcall layout-func (list :yield template-string)))
          template-string))))

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

(defun index ()
  (render "index.html"))

(defun projects ()
  (render "projects.html"))

(defun now ()
  (render "now.html"))

(defun reading ()
  (render "reading.html"))

(defun quicklinks ()
  (render "quicklinks.html"))

(defun make-coffee ()
  (setf (return-code*) 418)
  (render "418.html"))

(setf *dispatch-table*
  (nconc
    (create-folder-dispatcher-and-handler "/public/" #p"public/")
    (mapcar (lambda (args)
              (apply #'create-prefix-dispatcher args))
            '(("/" index)
              ("/projects" projects)
              ("/now" now)
              ("/reading" reading)
              ("/quicklinks" quicklinks)
              ("/make-coffee" make-coffee)))))

;;; --- Entrypoint ---

(defparameter *port* 4000)

(defparameter *content-directory* #p"content/")

(defparameter *layout-directory* #p"layouts/")

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
