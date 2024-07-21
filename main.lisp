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
                :return-code*
                :*dispatch-table*)
  (:import-from :cl-who
                :with-html-output-to-string
                :str
                :html-mode)
  (:import-from :bt
                :join-thread
                :all-threads
                :thread-name)
  (:import-from :uiop
                :quit)
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
               (:li (:a :href "/projects" "Projects"))
               (:li (:a :href "/reading" "Reading"))
               (:li (:a :href "/now" "/now")))))
         (:main
           ,@body)
         (:hr)
         (:footer
           (:p "I acknowledge the Wurundjeri people as the Traditional
                custodians of the land on which I live, work, create, play and
                learn. I pay my respects to their elders past and present, and
                to all First Nations people and communities.")
           (:p "The source code for this website can be found "
               (:a :href "https://github.com/aronlebani/lebani.dev.git" :target "_blank" "here")
               ".")
           (:p "(c) " (str (get-year)) " Aron Lebani"))))))

(defmacro error-layout ((&key title) &body body)
  `(with-html-output-to-string (out nil :prologue t)
     (:html :lang "en"
       (:head
         (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
         (:meta :name "viewport" :content "width=device-width, height=device-height, initial-scale=1.0, minimum-scale=1.0")
         (:title ,title)
         (:link :rel "stylesheet" :type "text/css" :href "public/index.css")
         (:link :rel "icon" :type "image/png" :href "public/favicon-16x16.png" :sizes "16x16")
         (:link :rel "icon" :type "image/png" :href "public/favicon-32x32.png" :sizes "32x32"))
       (:body
         (:main
           ,@body)))))

;;; --- Pages ---

(defroute (index "/")
  (layout (:title "Aron Lebani")
    (:h1 "Hello, I'm Aron.")
    (:p "I'm a computer programmer & a creative based in Melbourne (Naarm). I
         make websites and other software for creatives, local businesses,
         not-for-profits, and the community. You can get in touch with me at "
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
      (:li
        "check out the " (:a :href "/projects" "projects") " page for more..."))
    (:p "Things I love: computers, art, philosophy, nature. Things I want to do
         more of: build and contribute to open-source projects, make fun
         websites for awesome people, learn lots.")
    (:p "Here are the places you can find me online.")
    (:ul :class "horizontal-list"
      (:li
        (:a :href "mailto:aron@lebani.dev" :target "_blank" "Email"))   
      (:li
        (:a :href "https://github.com/aronlebani" :target "_blank" "Github"))
      (:li
        (:a :href "https://au.linkedin.com/in/aron-lebani-50861890" :target "_blank" "Linkedin")))))

(defroute (projects "/projects")
  (layout (:title "Projects")
    (:h1 "Projects")
    (:p "Here are some links to some of the projects that I've worked on.")
    (:p (:a :href "https://rowenawise.com" :target "_blank" "rowenawise.com")
        (:br)
        "Website for Rowena Wise.")
    (:p (:a :href "https://didirri.com" :target "_blank" "didirri.com")
        (:br)
        "Website for Didirri.")
    (:p (:a :href "https://everybodystryingpodcast.com" :target "_blank" "everybodystryingpodcast.com")
        (:br)
        "Website for Everybody's Trying To Find Their Way Home - a podcast by
         Jen Cloher about Māori and First Nations songwriters who are writing
         and performing in their tribal languages.")
    (:p (:a :href "https://instagram.com/lebani.amps" :target "_blank" "lebani.amps")
        (:br)
        "Hand made, portable beer-can guitar amps.")
    (:p (:a :href "https://addons.mozilla.org/en-US/firefox/addon/hummingbird/" :target "_blank" "Hummingbird")
        (:br)
        "A browser extension that helps you find your bookmarks faster.")
    (:p (:a :href "https://crates.io/crates/nanohttp" :target "_blank" "nanohttp")
        (:br)
        "A small zero-dependency Rust library for parsing http requests and
         building valid http responses.")
    (:hr)
    (:h2 "Projects by others")
    (:p "Here are some other projects that I have participated in.")
    (:p (:a :href "https://youtu.be/n2yjaLo0QLc" :target "_blank" "Melbourne Indie Voices covers 15 Step by Radiohead")
        (:br)
        "I am in a choir called Melbourne Indie voices. We perform once a year
         and make occasional videos such as this one.")
    (:p (:a :href "https://www.youtube.com/watch?v=OsjVlPyl6TY" :target "_blank" "Nobody's Saviour - Rowena Wise")
        (:br)
        "I got to be a part of this incredible film clip for Nobody's Saviour
         by Rowena Wise.")))

(defroute (now "/now")
  (layout (:title "Now")
    (:h1 "/now")
    (:section :id "2024-07-21"
      (:h2 "July 21, 2024")
      (:h3 "Wabi-sabi and dotfiles")
      (:p "One of the things I love about computers and open-source software is
           that you can customize " (:em "everything") ". If you don't like how
           something looks or functions, you can just tweak it to your hearts
           content. If the software doesn't offer a config option for it (which
           is rare, as a lot of open-source software is extremely flexible),
           you can always modify the source code yourself. There is no end. In
           a world where I constantly feel overwhelmed by how complex things
           are, and how little control I have of the world around me, this is a
           very appealing respite.")
      (:p "I've recently found myself with a lot more free time than usual, and
           I've been spending a lot of time customising my "
           (:a :href "https://github.com/aronlebani/dotfiles" "dotfiles") ".
           I've also been thinking a lot about the Japanese philosophy of "
           (:a :href "https://en.wikipedia.org/wiki/Wabi-sabi" "Wabi-sabi") ".
           By the work of the higher powers that be, this "
           (:a :href "https://arkadiuszchmura.com/posts/stop-perfecting-your-config" "article")
          " came into my RSS feed the other day. It was a bit of a much needed
           slap in the face. It talks about why we should stop spending so
           much time configuring everything, and even delves into the deeper,
           more philosophical reasons for doing this.")
      (:p "There is a " (:a :href "https://buttondown.email/hillelwayne/archive/keep-perfecting-your-config" "counter argument")
          " to this. Which is only sort of a counter argument, in that it
           argues that it's okay to spend time making tweaks which make you
           more productive, but not to spend time making superficial tweaks
           such as colours and aesthetics")
      (:p "I'm also a creative, and aesthetics are important to me, which makes
           this a hard pill to swallow. Having an environment that appeals to
           me aesthetically makes me happier, so surely more productive? But
           alas, the Wabi-sabi principle applies specifically to aesthetics.
           There is beauty in inconsistency, in imperfection. Some of my best
           art comes from the tightest of constraints. So maybe I should
           embrace this?")
      (:p "I took a little advice from both articles - I stripped a bunch of
           stuff out of my dotfiles which I don't need. I'm rolling with the
           imperfect defaults. I made one final commit - "
           (:a :href "https://github.com/aronlebani/dotfiles/commit/7d5573d6ed7b1a209ec400af2dfe812181a2fa99" "Wabi-sabi (7d5573d)") ".
           My goal is to not make another commit for at least a month.
           Hopefully putting this short story out into the world will hold me
           accountable to that goal."))
    (:section :id "2024-07-20"
      (:h2 "July 20, 2024")
      (:p "We're right in the thick of winter here in Melbourne. Recently I
           asked a dear friend of mine to make a list of 10 things they love
           about winter, in order to help stay positive in this dreary time of
           year. I thought maybe I should make one of my own, so here it is.")
      (:h3 "10 things I love about winter")
      (:ol
        (:li "Going for a walk at night after it's been raining. Seeing the
              glistening of all the different coloured lights reflecting off
              the water on the streets.")
        (:li "Listening to records. The soft crackle of old vinyl breaking
              the moody silence between tracks, for some reason sounds so much
              better in winter.")
        (:li "The food.")
        (:li "The outfits.")
        (:li "Endless cups of tea.")
        (:li "An excuse to stay at home and read or write or watch a movie.")
        (:li "Rugging up with blankets and scarfes and gloves and sharing music
              and stories around a fire at night time.")
        (:li "The feeling of getting home after a freezing cold walk or bike
              ride. Slowly defrosting when your nose and ears are numb from
              the cold.")
        (:li "Waking up, making coffee, crawling back into bed to drink your
              coffee.")
        (:li "Moody music while it's pouring with rain outside. Listening to
              the sound of the rain, feeling grateful for being warm and dry,
              feeling the mood in your whole body.")
        (:li "That singular moment when the sun comes out from hiding. Feeling
              it's rays penetrating deep into your body and warming your
              soul.")))
    (:section :id "2024-05-15"
      (:h2 "May 15, 2024")
      (:p "I discovered a handy alias.")
      (:figure
        (:pre
          (:code
            "sudo_roulette() {
    if [ $ ((1 + $RANDOM % 100)) == 99 ]; then
        sudo rm -rf /
    else
        sudo $@
    fi
}

alias sudo=sudo_roulette"))
        (:figurecaption "Please don't actually do this :grimace:")))
    (:section :id "2024-04-28"
      (:h2 "April 28, 2024")
      (:p
        (:a :href "/make-coffee" "Make some coffee")))
    (:section :id "2024-04-09"
     (:h2 "April 9, 2024")
     (:p "TIL: The characters " (:code "^") " and " (:code "$") " used to move
          to the start and end of the line in vim are the same as the regex
          characters that match against the start and end of a string."))
    (:section :id "2024-04-07"
      (:h2 "April 7, 2024")
      (:p "I plan on using this space to share small updates with the world. It
           might be something short I've written, what I'm up to at the moment,
           the latest book I'm reading, a photo I've taken... I'll try to keep
           adding to it over time. See how we go."))))

(defroute (reading "/reading")
  (layout (:title "Reading")
    (:h1 "Reading")
    (:p "This is a list of books that changed my life. I've tried to keep it as
         short as possible.")
    (:ul
      (:li "Just Kids - " (:i "Patti Smith"))
      (:li "Zen and the Art of Motorcycle Maintenance - " (:i "Robert M. Pirsig"))
      (:li "The Little Prince - " (:i "Antoine de Saint-Exupéry"))   
      (:li "Quiet - " (:i "Susan Cain")))
    (:p "Some of my other favourite books... (in no particular order)")
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
        (:a :href "https://github.com/aronlebani?tab=repositories" :target "_blank" "Github"))
      (:li
        (:a :href "https://www.shazam.com" :target "_blank" "Shazam"))
      (:li
        (:a :href "https://www.spanishdict.com" :target "_blank" "Spanish Dict"))
      (:li
        (:a :href "https://melbourneindievoices.com.au/members" :target "_blank" "MIV"))
      (:li
        (:a :href "http://www.bom.gov.au/vic/forecasts/melbourne.shtml" :target "_blank" "Melbourne forecast")))))

(defroute (make-coffee "/make-coffee")
  (setf (return-code*) 418)
  (error-layout (:title "Error 418: I'm a teapot")
    (:h1 "Error 418: I'm a teapot.")
    (:p "Tip me over and pour me out." (:br) (:a :href "/" "Back to homepage")
        ".")
    (:small
      (:a :href "https://en.wikipedia.org/wiki/Hyper_Text_Coffee_Pot_Control_Protocol" "Why am I seeing this?")
      " Find out "
      (:a :href "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/418" "more")
      ".")))

;;; --- Serve static files ---

(defstatic "/public/" #p"public/")

;;; --- Entrypoint ---

(defparameter *port* 4000)

(defvar *server*
  (make-instance 'easy-acceptor :port *port*))

(defun main ()
  (format t "Hunchentoot server is started~&")
  (format t "Listenting on localhost:~a~&" *port*)
  (setf (html-mode) :html5)
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
