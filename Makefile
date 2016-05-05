all:
	cd www/chat;make
	cd www/ui;make

clean:
	cd www/chat;make clean
	cd www/ui;make clean
