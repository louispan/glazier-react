SHELL := /bin/bash
INSTALL_ROOT:=$(shell stack path --local-install-root)
PROJECT_ROOT:=$(shell stack path --project-root)
GHCJS_OUT:=$(INSTALL_ROOT)/bin/glazier-react-todo.jsexe

.PHONY: all install install-min clean start stack-build

all: install
	@true # dependency tracking doesn't work for empty @targets

# install: build/todo.js build/todo.min.js
# 	npm install

install: build/todo.js src/Shim.js
	npm install

install-min: build/todo.min.js install
	@true # dependency tracking doesn't work for empty @targets

clean:
	rm -rf node_modules
	rm -rf build

start: install
	npm start

stack-build:
	stack build

$(GHCJS_OUT)/all.js: stack-build
	@true # dependency tracking doesn't work for empty @targets

$(GHCJS_OUT)/all.js.externs: stack-build
	@true # dependency tracking doesn't work for empty @targets

# add a nodeJS module wrapper http://edsko.net/2015/02/14/atom-haskell/
# Don't output to src as babel looks in that directory for js files to transpile.
# Babel will freeze trying to transpile ghcjs output.
# The nodeJS module wrapper will cause closure compiler fails with
# "ERROR - Closure dependency methods(goog.provide, goog.require, etc)
# must be called at file scope. "
# This step also removes the dummy calls to goog.provide and goog.require.
build/todo.js: $(GHCJS_OUT)/all.js haskell/jsexports/exports.header haskell/jsexports/exports.js
	mkdir -p build
	cat haskell/jsexports/exports.header \
		<(sed -e '/goog.provide.*/d;/goog.require.*/d' $(GHCJS_OUT)/all.js) \
		haskell/jsexports/exports.js \
		> build/todo.js

build/todo.min.js: build/react-externs.js $(GHCJS_OUT)/all.js.externs build/todo.js
	closure-compiler \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		--externs build/react-externs.js \
		--externs $(GHCJS_OUT)/all.js.externs \
		--create_source_map build/todo.min.js.map \
		--js build/todo.js \
		--js_output_file build/todo.min.js

build/react-externs.js:
	wget https://raw.githubusercontent.com/steida/react-externs/master/externs.js -O build/react-externs.js

src/Shim.js: $(PROJECT_ROOT)/jsextras/Shim.js
	cp $(PROJECT_ROOT)/jsextras/Shim.js src
