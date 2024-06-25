include .env

LISP = sbcl
EXE = server
SERVER = ${SERVER}
DEST = ${DEST}

.PHONY: debug build clean deploy

debug:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

build:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(asdf:make :website)' \
		--eval '(quit)'

clean:
	rm $(EXE)

deploy:
	chmod +x $(EXE)
	rsync -rvsp --delete --progress public $(EXE) $(SERVER):$(DEST)
	ssh $(SERVER) 'systemctl restart $(EXE).service'
