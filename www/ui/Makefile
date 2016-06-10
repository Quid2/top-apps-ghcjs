.PHONY: all dev build clean
PROJECT:=ui
INSTALL_ROOT:=$(shell stack path --local-install-root)
JSALL:=$(INSTALL_ROOT)/bin/$(PROJECT).jsexe/all.js

slow: $(PROJ_DIR)/all.min.js

fast: $(PROJ_DIR)/all.js

dev:
	cd ../../;stack build --file-watch --exec 'make -f www/$(PROJECT)/Makefile PROJ_DIR=$(CURDIR) fast'

build:
	cd ../../;stack build

clean:
	rm all.js all.min.js

$(PROJ_DIR)/all.min.js: $(PROJ_DIR)/all.js
	sed -i "" -e 's/goog.provide.*//' $<
	sed -i "" -e 's/goog.require.*//' $<
  # Advanced optimizations won't work with 'top'
	ccjs $< --compilation_level=SIMPLE_OPTIMIZATIONS > $@

$(PROJ_DIR)/all.js: $(JSALL)
	echo "(function(global,React,ReactDOM) {" > $@
	cat $(JSALL) >> $@
	echo "})(window, window['React'], window['ReactDOM']);" >> $@

