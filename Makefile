PUBLIC = public/*
SCRIPT = layouts config.ru *.rb
HOST = lebani_dev
DEST = /var/www/lebani.dev

default: view

debug:
	nanoc compile && rackup

build:
	nanoc compile

view: build
	nanoc view

upload: build
	rsync -rvsp --delete $(SCRIPT) $(HOST):$(DEST)/script
	rsync -rvzp --delete $(PUBLIC) $(HOST):$(DEST)/public

.PHONY: debug build upload view
