#-*- Mode: makefile -*- 
# Release: PVS2Why-0.1 (11/10/07)
#---

#--
package=PVS2Why
require=ProofLite
PVS=pvs
#PVS_DIR=
#PVS_OPTIONS=
#--

COLON=:
COMMA=,
#--

packstamp=$(shell head -n 1 lib/version)
fullpack=$(shell pwd)
libpath=$(subst $(COLON), ,$(PVS_LIBRARY_PATH):$(PVS_DIR)/lib)
listreq=$(if $(require),$(subst $(COMMA), -or -name ,-name $(require)),)
#precond=$(shell find $(libpath) -follow $(listreq) 2>/dev/null)
pvscond=$(shell test -f $(PVS_DIR)/$(PVS) && echo OK)
proveit=$(shell find $(PVS_DIR) -name proveit -print 2>/dev/null)
pvspack=$(shell cd ..;pwd)
libpack=$(shell find $(libpath) -follow -type d -regex $(pvspack) 2>/dev/null)
thepack=$(shell test -L ../$(package) && echo OK)

all: install

check:  checkerror
	@echo 
	@echo "    +---"
	@echo "    | Package: $(packstamp)"
	@echo "    | Package Directory: $(fullpack)"
	@echo "    | $(PVS_VER) (Version 4.0 or higher is required)"
	@echo "    | PVS_LIBRARY_PATH=$(PVS_LIBRARY_PATH)"
	@echo "    +--"

checkerror: 
ifndef FORCE
ifndef PVS_DIR
ERRMSG=Either define an environment variable PVS_DIR or call make PVS_DIR=<dir>, where <dir> is the directory where PVS resides
else
ifeq (,$(pvscond))
ERRMSG=PVS [= $(PVS)] not found in PVS_DIR [= $(PVS_DIR)]. Check definition of PVS_DIR or call make PVS=<pvs>
else
PVS_VER=$(shell $(PVS_DIR)/$(PVS) -version)
#ifeq (,$(precond))
#ERRMSG=Required package(s) in $(require) does (do) not seem to be installed
#else
ifeq (,$(libpack))
WRNMSG= "** Warning ** Make sure that PVS_LIBRARY_PATH includes $(pvspack) before using PVS2Why!"
endif
endif
endif
endif
#endif
ifdef ERRMSG
$(error Error: $(ERRMSG))
endif

install: check pvs2why 
	@echo $(WRNMSG)	
	@./installib.sh $(package)

pvs2why: 
	@sed -e "s,CHANGE_TO_PVS_DIR,$(PVS_DIR)," -e "s,CHANGE_TO_PROOFLITE_DIR,$(fullpack)/lib," < src/pvs2why.sh > $(PVS_DIR)/pvs2why
	@chmod a+x $(PVS_DIR)/pvs2why
	@echo "*** Note: The script pvs2why has been installed at $(PVS_DIR)"

test: forcethis
ifneq (OK,$(thepack))
	@echo "Error: Package $(package) does not seem to be properly installed"
else
	cd java;make 
	cp java/PVS2Java.jar examples
	cd examples;make PVS_DIR=$(PVS_DIR)
endif

forcethis:



