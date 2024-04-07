LISP ?= sbcl

.PHONY: run build clean deploy deploy-dry-run

run:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(website:main)'

build:
	$(LISP) --load website.asd \
		--eval '(ql:quickload :website)' \
		--eval '(asdf:make :website)' \
		--eval '(quit)'

clean:
	rm server

deploy:
	chmod -R +x server
	rsync -rvsp --delete --progress \
		public server root@lebani.dev:/var/lebani.dev/www
