LYS_BACKEND?=opencl
LYS_FRONTEND?=sdl

-include custom.mk

all: explorer/explorer designer/designer README.md

.PHONY: explorer/explorer
explorer/explorer:
	LYS_BACKEND=$(LYS_BACKEND) LYS_FRONTEND=$(LYS_FRONTEND) $(MAKE) -C explorer explorer

.PHONY: designer/designer
designer/designer:
	LYS_BACKEND=$(LYS_BACKEND) LYS_FRONTEND=$(LYS_FRONTEND) $(MAKE) -C designer designer

README.md: $(shell find lib src -name \*.fut; ls *.fut)
	futhark literate --backend=$(LYS_BACKEND) --stop-on-error README.fut

clean:
	$(MAKE) -C explorer clean
	$(MAKE) -C designer clean
	rm -f README.c README
