.PHONY: all dev build clean
PROJECT:=chat
INSTALL_ROOT:=$(shell stack path --local-install-root)
JSALL:=$(INSTALL_ROOT)/bin/$(PROJECT).jsexe/all.js

all: build all.min.js

dev:
	cd ../../;stack build --file-watch --exec 'make -f www/$(PROJECT)/Makefile all.js'

build:
	cd ../../;stack build

clean:
	rm all.js all.min.js

all.min.js: all.js
	sed -i "" -e 's/goog.provide.*//' all.js
	sed -i "" -e 's/goog.require.*//' all.js
  # Advanced optimizations won't work with quid2.net
	ccjs all.js --compilation_level=SIMPLE_OPTIMIZATIONS > all.min.js

all.js: $(JSALL)
	echo "(function(global,React,ReactDOM) {" > all.js
	cat $(JSALL) >> all.js
	echo "})(window, window['React'], window['ReactDOM']);" >> all.js

