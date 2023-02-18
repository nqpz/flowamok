LYS_BACKEND?=opencl
LYS_FRONTEND?=sdl

-include custom.mk

all: explorer/explorer README.md

.PHONY: explorer/explorer
explorer/explorer:
	LYS_BACKEND=$(LYS_BACKEND) LYS_FRONTEND=$(LYS_FRONTEND) $(MAKE) -C explorer explorer

README.md: $(shell find lib src -name \*.fut; ls *.fut)
	futhark literate --backend=$(LYS_BACKEND) --stop-on-error README.fut

clean:
	$(MAKE) -C explorer clean
	rm -f README.c README
