all: explorer/explorer README.md

explorer/explorer:
	$(MAKE) -C explorer explorer

README.md: $(shell find -name \*.fut)
	futhark literate --backend=$(LYS_BACKEND) --stop-on-error README.fut

clean:
	$(MAKE) -C explorer clean
	rm -f README.c README
