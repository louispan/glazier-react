# This makefile is for building using GHCJS
SHELL := /bin/bash

.PHONY: all install clean build

all: build
	@true # dependency tracking doesn't work for empty @targets

stack.ghcjs.yaml: stack.yaml ghcjs.yaml
	cat stack.yaml ghcjs.yaml > stack.ghcjs.yaml

build: stack.ghcjs.yaml
	stack build --stack-yaml stack.ghcjs.yaml

clean: stack.ghcjs.yaml
	stack clean --stack-yaml stack.ghcjs.yaml


SHELL := /bin/bash

PROJECT_NAME := javascript-extras-test

include ghcjs.mk

.PHONY: clean install start

install: build
	npm install

clean::
	rm -rf node_modules

start: | install
	node_modules/.bin/webpack-dev-server --content-base public
