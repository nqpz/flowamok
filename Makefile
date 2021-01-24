PROGNAME=flowamok
include lib/github.com/diku-dk/lys/common.mk

README.md: $(PROG_FUT_DEPS)
	futhark literate --backend=$(LYS_BACKEND) --stop-on-error README.fut
