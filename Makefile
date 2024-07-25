include .env

LISP = sbcl

.PHONY: debug start build clean deploy

debug:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

start:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

clean:
	rm *.fasl

deploy:
	rsync -rvsp \
		--delete \
		--progress \
		README.md LICENSE website.asd Makefile main.lisp public templates articles \
		${SERVER}:${DEST}
	ssh ${SERVER} 'systemctl restart website.service'
