SHELL := /bin/bash

PROJECT_NAME := glazier-react

include ghcjs.mk

.PHONY: clean install start

install: build
	npm install

clean::
	rm -rf node_modules

start: | install
	node_modules/.bin/webpack-dev-server --content-base public
