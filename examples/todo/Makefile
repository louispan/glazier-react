.PHONY: all clean

INSTALL_ROOT:=$(shell stack path --local-install-root)

all: install

install-todo: glazier-react-module
	npm install

build-glazier-react:
	stack build

# add a nodeJS module wrapper http://edsko.net/2015/02/14/atom-haskell/
# Don't output to src as babel looks in that directory for js files to transpile.
# Babel will freeze trying to transpile ghcjs output.
glazier-react-module: build-glazier-react
	mkdir -p build
	cat haskell/jsexports/exports.header \
        $(INSTALL_ROOT)/bin/glazier-react-todo.jsexe/all.js \
        haskell/jsexports/exports.js \
        haskell/jsexports/exports.footer > build/todo.js

install: install-todo

clean:
	rm -rf node_modules
	rm -rf build
