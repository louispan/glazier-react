SHELL := /bin/bash

ifndef PROJECT_NAME
$(error PROJECT_NAME is not set)
endif

# Run make with MINIFY=true to make minified js
ifeq ($(MINIFY),)
	PROJECT_SOURCE=$(PROJECT_NAME).max.js
else
	PROJECT_SOURCE=$(PROJECT_NAME).min.js
endif
export MINIFY

PROJECT_ROOT := $(shell stack path --project-root)

# Avoid clashing with react-start-app build dir
BUILD_DIR := .build-ghcjs

EXTERNS := $(GHCJS_PROJECT_OUT)/all.js.externs

$(shell cat "$(PROJECT_ROOT)/stack.yaml" "$(PROJECT_ROOT)/ghcjs.yaml" > "$(PROJECT_ROOT)/stack.ghcjs.yaml")
GHCJS_INSTALL_ROOT := $(shell stack --stack-yaml $(PROJECT_ROOT)/stack.ghcjs.yaml path --local-install-root)
GHCJS_PROJECT_OUT := $(GHCJS_INSTALL_ROOT)/bin/$(PROJECT_NAME).jsexe

.PHONY: ghcjs.mk $(BUILD_DIR) clean

ghcjs.mk: $(PROJECT_ROOT)/stack.ghcjs.yaml
	stack --stack-yaml $(PROJECT_ROOT)/stack.ghcjs.yaml build
	@# touch ghcjs.mk which is included, forcing Makefile to revaluate targets with new timestamps
	@touch ghcjs.mk

clean::
	stack --stack-yaml "$(PROJECT_ROOT)/stack.ghcjs.yaml" clean
	rm -rf $(BUILD_DIR)

# make all build directories
$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

build: $(BUILD_DIR)/$(PROJECT_NAME).js | $(BUILD_DIR)

# The ghcjs compile output
$(GHCJS_PROJECT_OUT)/all.js $(GHCJS_PROJECT_OUT)/all.js.externs: | ghcjs.mk

# Create the project.js for webpack from either the min.js or max.js.
# The project.js can't be a symbolic link, otherwise webpack can't resolve dependencies
$(BUILD_DIR)/$(PROJECT_NAME).js: $(BUILD_DIR)/$(PROJECT_SOURCE) | $(BUILD_DIR)
	cp $(BUILD_DIR)/$(PROJECT_SOURCE) $(BUILD_DIR)/$(PROJECT_NAME).js

# Don't output to 'src' as babel looks in that directory for js files to transpile.
# Babel will freeze trying to transpile ghcjs output.
$(BUILD_DIR)/$(PROJECT_NAME).max.js: $(GHCJS_PROJECT_OUT)/all.js | $(BUILD_DIR)
	cp $(GHCJS_PROJECT_OUT)/all.js $(BUILD_DIR)/$(PROJECT_NAME).max.js

$(BUILD_DIR)/$(PROJECT_NAME).min.js: $(EXTERNS) $(BUILD_DIR)/$(PROJECT_NAME).max.js
	closure-compiler \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		$(foreach extern, $(EXTERNS),--externs $(extern)) \
		--create_source_map build/$(PROJECT_NAME).min.js.map \
		--js $(BUILD_DIR)/$(PROJECT_NAME).max.js \
		--js_output_file $(BUILD_DIR)/$(PROJECT_NAME).min.js
