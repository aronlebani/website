include .env

LISP = sbcl
EXE = website

.PHONY: debug
debug:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

.PHONY: start
start:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

.PHONY: clean
clean:
	rm $(EXE) *.fasl

.PHONY: deploy
deploy:
	rsync -rvsp \
		--delete \
		--progress \
		README.md LICENSE website.asd Makefile main.lisp public templates articles \
		${SERVER}:${DEST}
	ssh ${SERVER} 'systemctl restart website.service'

.PHONY: build
build:
	$(LISP) --load website.asd \
    	--eval '(ql:quickload :website)' \
		--eval '(asdf:make :website)' \
		--eval '(quit)'
