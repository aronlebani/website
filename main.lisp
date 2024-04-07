;;;; main.lisp
;;;;
;;;; Website for https://lebani.dev
;;;;
;;;; Aron Lebani
 
(defpackage :website
  (:use :cl)
  (:import-from :hunchentoot
                :create-prefix-dispatcher
                :create-folder-dispatcher-and-handler
                :easy-acceptor
                :start
                :stop
                :*dispatch-table*)
  (:import-from :cl-who
                :with-html-output-to-string
                :str
                :html-mode)
  (:export :main))

(in-package :website)

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
  (nth 5 (multiple-value-list
           (decode-universal-time
             (get-universal-time)))))

;;; --- Layout ---

(defmacro layout ((&key title) &body body)
  `(with-html-output-to-string (out nil :prologue t)
     (:html :lang "en"
       (:head
         (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
         (:meta :name "viewport" :content "width=device-width, height=device-height, initial-scale=1.0, minimum-scale=1.0")
         (:title ,title)
         (:meta :name "decription" :content "This is my website.")
         (:meta :property "og:title" :content "Aron Lebani")
         (:meta :property "og:url" :content "https://lebani.dev")
         (:meta :property "og:type" :content "website")
         (:meta :property "og:description" :content "This is my website.")
         (:link :rel "stylesheet" :type "text/css" :href "public/index.css")
         (:link :rel "icon" :type "image/png" :href "public/favicon-16x16.png" :sizes "16x16")
         (:link :rel "icon" :type "image/png" :href "public/favicon-32x32.png" :sizes "32x32"))
       (:body
         (:header
           (:nav
             (:ul
               (:li (:a :href "/" "Home"))
               (:li (:a :href "/reading" "Reading"))
               (:li (:a :href "/now" "/now")))))
         (:main
           ,@body)
         (:hr)
         (:footer
           (:p
             "I acknowledge the Wurundjeri people as the Traditional custodians of
              the land on which I live, work, create, play and learn. I pay my
              respects to their elders past and present, and to all First Nations
              people and communities.")
           (:br)
           (:p
             "The source code for this website can be found "
             (:a :href "https://github.com/aronlebani/lebani.dev.git" :target "_blank" "here")
             ".")
           (:p "(c) " (str (get-year)) " Aron Lebani"))))))

;;; --- Pages ---

(defroute (index "/")
  (layout (:title "Aron Lebani")
    (:h1 "Hello, I'm Aron.")
    (:p
      "I'm a computer programmer & a creative. You can get in touch with me at "
      (:a :href "mailto:aron@lebani.dev" "aron@lebani.dev") ". Here are some
      things I've made.")
    (:ul
      (:li
        (:a :href "https://rowenawise.com" :target "_blank" "rowenawise.com"))
      (:li
        (:a :href "https://didirri.com" :target "_blank" "didirri.com"))
      (:li
        (:a :href "https://everybodystryingpodcast.com" :target "_blank" "everybodystryingpodcast.com"))
      (:li
        (:a :href "https://instagram.com/lebani.amps" :target "_blank" "lebani.amps"))
      (:li "more stuff in the works..."))
    (:p
      "Things I want to do more of: build awesome open-source projects; make
       more fun websites for awesome people; learn lots.")
    (:p
      "Here are the usual suspects.")
    (:ul
      (:li
        (:a :href "https://github.com/aronlebani" :target "_blank" "Github"))
      (:li
        (:a :href "https://au.linkedin.com/in/aron-lebani-50861890" :target "_blank" "Linkedin"))
      (:li
        (:a :href "https://instagram.com/lebani.amps" :target "_blank" "Instgram")))))

(defroute (now "/now")
  (layout (:title "Now")
    (:h1 "/now")
    (:p "April 7, 2024")
    (:p
      "I plan on using this space to share small updates with the world. It
       might be something short I've written, what I'm up to at the moment,
       the latest book I'm reading, a photo I've taken... I'll try to keep
       adding to it over time. See how we go.")))

(defroute (reading "/reading")
  (layout (:title "Reading")
    (:h1 "Reading")
    (:p
      "This is a list of books that changed my life. I've tried to keep it as
       short as possible.")
    (:ul
      (:li "Just Kids - " (:i "Patti Smith"))
      (:li "Zen and the Art of Motorcycle Maintenance - " (:i "Robert M. Pirsig"))
      (:li "The Little Prince - " (:i "Antoine de Saint-Exupéry"))   
      (:li "Quiet - " (:i "Susan Cain")))
    (:p
      "Some of my other favourite books... (in no particular order)")
    (:ul
      (:li "Entangled Life - " (:i "Merlin Sheldrake"))
      (:li "Monkey Grip - " (:i "Helen Garner"))
      (:li "Alice's Adventures in Wonderland - " (:i "Lewis Carroll"))
      (:li "Silence - " (:i "Thich Nhat Hanh"))
      (:li "High Fidelity - " (:i "Nick Hornby"))
      (:li "A Clockwork Orange - " (:i "Anthony Burgess"))
      (:li "How Music Works - " (:i "David Byrne"))
      (:li "Sapiens - " (:i "Yuval Noah Harari"))
      (:li "The Brain That Changes Itself - " (:i "Norman Doidge"))
      (:li "The Hitchhiker's Guide to the Galaxy - " (:i "Douglas Adams")))))

(defroute (quicklinks "/quicklinks")
  (layout (:title "Quicklinks")
    (:h1 "Quicklinks")
    (:ul
      (:li
        (:a :href "https://betaapp.fastmail.com/mail/Inbox/?u=18e1403b" :target "_blank" "Webmail"))
      (:li
        (:a :href "https://files.lebani.dev" :target "_blank" "Files"))
      (:li
        (:a :href "https://github.com/aronlebani?tab=repositories" :target "_blank" "Github"))
      (:li
        (:a :href "https://www.shazam.com" :target "_blank" "Shazam"))
      (:li
        (:a :href "https://www.spanishdict.com" :target "_blank" "Spanish Dict"))
      (:li
        (:a :href "https://melbourneindievoices.com.au/members" :target "_blank" "MIV"))
      (:li
        (:a :href "http://www.bom.gov.au/vic/forecasts/melbourne.shtml" :target "_blank" "Melbourne forecast"))
      (:li
        (:a :href "https://tramtracker.com.au" :target "_blank" "Tramtracker")))))

;;; --- Serve static files ---

(defstatic "/public/" #p"public/")

;;; --- Entrypoint ---

(defparameter *port* 4000)

(defvar *server* (make-instance 'easy-acceptor :port *port*))

(defun main ()
  (format t "Hunchentoot server is started~&")
  (format t "Listenting on localhost:~a~&" *port*)
  (setf (html-mode) :html5)
  (start *server*)
  (handler-case (bt:join-thread
                  (find-if (lambda (th)
                             (search "hunchentoot" (bt:thread-name th)))
                           (bt:all-threads)))
    ; Catch C-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (format *error-output* "Aborting~&")
          (stop *server*)
          (uiop:quit)))
    ; Something went wrong
    (error (c) (format t "An unknown error occured:~&~a~&" c))))
