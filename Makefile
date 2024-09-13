include .env

LISP = sbcl
EXE = website
FILES = website.asd Makefile main.lisp public templates articles

debug:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

start:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

clean:
	rm $(EXE) *.fasl

deploy:
	rsync -rvsp --delete $(FILES) $(SERVER):$(DEST)
	ssh $(SERVER) 'systemctl restart website.service'

build:
	$(LISP) --load website.asd \
    	--eval '(ql:quickload :website)' \
		--eval '(asdf:make :website)' \
		--eval '(quit)'

.PHONY: build deploy clean start debug
