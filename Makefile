FILES = public/*
HOST = lebani_dev
DEST = /var/www/lebani.dev

debug:
	nanoc compile --watch & nanoc view --live-reload

build:
	nanoc compile

upload: build
	rsync -rvzp --delete $(FILES) $(HOST):$(DEST)

.PHONY: debug build upload
