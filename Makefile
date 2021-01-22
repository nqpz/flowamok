PROGNAME=flowamok
include lib/github.com/diku-dk/lys/common.mk

README.md: README.fut
	futhark literate --backend=$(LYS_BACKEND) --stop-on-error README.fut
