.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: build
# js-build/ui.min.js

build: $(INSTALL_ROOT)
	- rm build
	ln -s $(INSTALL_ROOT)/bin/ui.jsexe build

dev:
	cd ../../;stack build --file-watch

build/ui.min.js: build/ui.js
	closure --compilation_level=ADVANCED_OPTIMIZATIONS build/ui.js > build/ui.min.js

build/ui.js: $(INSTALL_ROOT)/bin/ui.jsexe/all.js
	echo "(function(global,React,ReactDOM) {" > ui.js
	cat build/all.js >> ui.js
	echo "})(window, window['React'], window['ReactDOM']);" >> ui.js
	sed -i 's/goog.provide.*//' ui.js
	sed -i 's/goog.require.*//' ui.js

clean:
	rm -rf build
