.PHONY: all preparetests runtests
all : preparetests


# Can only run make after you run configure
# Some configuration parameters
include deputy/test/Makefile

ifdef NODEPUTY
 PERFCOMP:=gcc
else
 PERFCOMP:=$(DEPUTY) $(OPTIONS)
 ifndef FULLCHECKS
   PERFCOMP:=$(PERFCOMP) --fast-checks --fail-stop
 endif
endif



preparetests:
	$(MAKE) -C deputy

ifndef TESTARG
 TESTARG=--run
endif

runtests: 
# Now run the CIL regression tester, but with the extra tests
	cd $(DEPUTYDIR)/test && \
          DEPUTYEXTRATESTS=$(DEPUTYDIR)/../deputyextratests.pl \
          ./testdeputy --timeout=120 $(TESTARG)


clean: 
	$(MAKE) -C deputy clean


###
### 
###



#
# OLDEN tests
# named by the name of the directory

%-noclean: olden/%
	cd olden/$* && $(MAKE) CC="$(PERFCOMP)"
	cd olden/$* && sh ./testit

%-clean: olden/%
	cd olden/$* && $(MAKE) clean

%: olden/%
	$(MAKE) $*-clean $*-noclean

# Run without Deputy 
%-base: olden/%
	$(MAKE) $* NODEPUTY=1




# 
# PTRDIST
#
%-clean: ptrdist/%
	cd ptrdist/$* && $(MAKE) clean

%-noclean: ptrdist/%
	cd ptrdist/$* && $(MAKE) CC="$(PERFCOMP)"
	cd ptrdist/$* && make test

%: ptrdist/%
	$(MAKE) $*-clean $*-noclean

%-base: ptrdist/%
	$(MAKE) $* NODEPUTY=1



#$*-clean $*-noclean

%-base: ptrdist/%
	$(MAKE) $* NODEPUTY=1



# 
# Spec95.
#
%-clean: spec95/%
	cd spec95/$*/src && $(MAKE) clean

%-noclean: spec95/%
	cd spec95/$*/src && $(MAKE) CC="$(PERFCOMP)" build
	cd spec95/$*/src && make test

%: spec95/%
	$(MAKE) $*-clean $*-noclean

%-base: spec95/%
	$(MAKE) $* NODEPUTY=1


# 
# Spec 2000
#
%-clean: spec00/%
	cd spec00/$*/src && $(MAKE) clean

%-noclean: spec00/%
	cd spec00/$*/src && $(MAKE) CC="$(PERFCOMP)" build
	cd spec00/$*/src && make test

%: spec00/%
	$(MAKE) $*-clean $*-noclean

%-base: spec00/%
	$(MAKE) $* NODEPUTY=1


# 
# Mediabench
#
%-clean: mediabench/%
	cd mediabench/$* && $(MAKE) clean

%-noclean: mediabench/%
	cd mediabench/$* && $(MAKE) CC="$(PERFCOMP)" build
	cd mediabench/$* && make test

%: mediabench/%
	$(MAKE) $*-clean $*-noclean

%-base: mediabench/%
	$(MAKE) $* NODEPUTY=1


#
# PERFORMANCE
#

perftest/%-base: perf/%.c 
	$(MAKE) perftest/$* NODEPUTY=1

perftest/%: perf/%.c
	cd perf && rm -f $*.exe
	cd perf && $(PERFCOMP) -O3 $*.c -o $*.exe
	cd perf && sh -c "time ./$*.exe"
