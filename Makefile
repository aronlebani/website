LISP = sbcl
EXE = server
SERVER = root@lebani.dev
DEST = /var/lebani.dev/www

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
