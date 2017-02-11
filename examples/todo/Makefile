.PHONY: all clean

INSTALL_ROOT:=$(shell stack path --local-install-root)

all: install

install-todo:
	npm install

install-glazier-react:
	stack build
	cp "$(INSTALL_ROOT)/bin/glazier-react-exe.jsexe/all.js" public

install: install-glazier-react install-todo

clean:
	rm -rf node_modules
