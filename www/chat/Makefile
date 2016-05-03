.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: build

build: $(INSTALL_ROOT)
	- rm build
	ln -s $(INSTALL_ROOT)/bin/chat.jsexe build

dev:
	cd ../../;stack build --file-watch

clean:
	rm -rf build
