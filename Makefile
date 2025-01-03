FILES = public/*
HOST = lebani_dev
DEST = /var/www/lebani.dev

default: view

debug:
	nanoc compile --watch & nanoc view --live-reload

build:
	nanoc compile

view: build
	nanoc view

upload: build
	rsync -rvzp --delete $(FILES) $(HOST):$(DEST)/public

.PHONY: debug build upload view
