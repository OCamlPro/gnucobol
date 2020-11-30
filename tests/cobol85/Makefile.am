#
# Makefile gnucobol/tests/cobol85
#
# Copyright (C) 2003-2012, 2015-2020 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Simon Sobisch
#
# This file is part of GnuCOBOL.
#
# The GnuCOBOL compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GnuCOBOL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.

# FIXME: doesn't work when EXEC85$(EXEEXT) exists in sourcedir but not
#        in builddir

RM = rm -rf

MODULES_ALL = NC SM IC SQ RL ST SG OB IF RW DB IX DBNOIX CM

# currently untested modules: CM
if COB_MAKE_IX
MODULES = NC SM IC SQ RL ST SG OB IF RW DB IX
MODULES_RUN = NC_RUN SM_RUN IC_RUN SQ_RUN RL_RUN \
	ST_RUN SG_RUN OB_RUN IF_RUN RW_RUN DB_RUN IX_RUN
SUMMARY = summary.txt
else
MODULES = NC SM IC SQ RL ST SG OB IF RW DBNOIX
MODULES_RUN = NC_RUN SM_RUN IC_RUN SQ_RUN RL_RUN \
	ST_RUN SG_RUN OB_RUN IF_RUN RW_RUN DBNOIX_RUN
SUMMARY = summarynoix.txt
endif

URL_NEWCOB_Z = https://www.itl.nist.gov/div897/ctg/suites/newcob.val.Z
URL_NEWCOB_TAR_GZ = https://sourceforge.net/projects/gnucobol/files/nist/newcob.val.tar.gz/download

EXTRA_DIST = EXEC85.conf.in expand.pl report.pl summary.pl summary.txt \
	summarynoix.txt NC.txt SM.txt IC.txt SQ.txt RL.txt IX.txt \
	ST.txt SG.txt OB.txt IF.txt RW.txt DB.txt DBNOIX.txt Makefile.module.in

#CLEANFILES = EXEC85$(EXEEXT) summary.log

CURL_FLAGS = 
WGET_FLAGS = -t1 -T5
DIFF_FLAGS = @DIFF_FLAGS@
COBC_FLAGS = -std=cobol85 -debug

# MAKEFLAGS = --no-print-directory

# targets that are only logical targets instead of files
.PHONY: test diff-summary diff summary.log $(MODULES_RUN) unpack-Z unpack-gz

NC_RUN: NC
	@cd NC && $(MAKE) -k

SM_RUN: SM
	@cd SM && $(MAKE) -k

IC_RUN: IC
	@cd IC && $(MAKE) -k

SQ_RUN: SQ
	@cd SQ && $(MAKE) -k

RL_RUN: RL
	@cd RL && $(MAKE) -k

ST_RUN: ST
	@cd ST && $(MAKE) -k

SG_RUN: SG
	@cd SG && $(MAKE) -k

OB_RUN: OB
	@cd OB && $(MAKE) -k

IF_RUN: IF
	@cd IF && $(MAKE) -k

RW_RUN: RW
	@cd RW && $(MAKE) -k

DB_RUN: DB
	@cd DB && $(MAKE) -k

IX_RUN: IX
	@cd IX && $(MAKE) -k

DBNOIX_RUN: DBNOIX
	@cd DBNOIX && $(MAKE) -k

CM_RUN: CM
	@cd CM && $(MAKE) -k

modules: $(MODULES_ALL)

duration.log:
	@echo "Collecting durations for each module..."
	@echo "Duration for modules: $(MODULES)" > $@
	@echo "" >> $@
	@for m in $(MODULES); do \
		cat "$$m/duration.txt" >> $@; \
	done
	@echo Done

test: $(MODULES_RUN)
	@echo
	$(MAKE) diff-summary

summary.log:
	@echo "Computing total test results..."
	@perl "$(srcdir)/summary.pl" $(MODULES) > $@

diff:
	@echo "Comparing test results for each module"
	@retd=0; for m in $(MODULES); do \
		echo "diff $$m/report.txt..."; \
		diff $(DIFF_FLAGS) "$(srcdir)/$$m.txt" "$$m/report.txt"; \
		ret=$$? && if test $$ret -gt $$retd; then retd=$$ret; fi \
	done; \
		if test $$retd -gt 1; then exit $$retd; fi
	@echo Done
	$(MAKE) diff-summary

diff-summary:
	$(MAKE) summary.log
	@echo "Comparing total test results..."
	@echo "diff $(SUMMARY)..."
	@diff $(DIFF_FLAGS) "$(srcdir)/$(SUMMARY)" "summary.log"
	@echo Done

newcob.val.Z:
	@echo "Trying to download newcob.val.Z..."
	@(which curl 1>/dev/null && curl $(CURL_FLAGS) "$(URL_NEWCOB_Z)" -o newcob.val.Z) || \
	 wget $(WGET_FLAGS) "$(URL_NEWCOB_Z)"

newcob.val.tar.gz:
	@echo "Trying to download newcob.val.tar.gz..."
	@(which curl 1>/dev/null && curl $(CURL_FLAGS) "$(URL_NEWCOB_TAR_GZ)" -o newcob.val.tar.gz) || \
	 wget $(WGET_FLAGS) -O $@ "$(URL_NEWCOB_TAR_GZ)"

unpack-Z: newcob.val.Z
	@echo "Unpacking $<..."
	@gunzip -f "$<" || gzip -d -f "$<" || uncompress "$<"

unpack-gz: newcob.val.tar.gz
	@echo "Unpacking $<..."
	@tar -xf "$<"

# always copy pack from srcdir for in-place unpack, if possible
newcob.val:
	@if test -f "$(srcdir)/newcob.val.tar.gz"; then \
		if test ! -f "$(abs_builddir)/newcob.val.tar.gz"; then \
			export pack=newcob.val.tar.gz &&                 \
			echo "Copying $$pack to current directory..." && \
			cp "$(srcdir)/$$pack" . ;                        \
		fi;                                             \
		$(MAKE) unpack-gz;                              \
	else \
		if test -f "$(srcdir)/newcob.val.Z"; then            \
			if test ! -f "$(abs_builddir)/newcob.val.Z"; then \
				export pack=newcob.val.Z &&                      \
				echo "Copying $$pack to current directory..." && \
				cp "$(srcdir)/$$pack" . ;                        \
			fi;                                             \
			$(MAKE) unpack-Z; \
		else \
			($(MAKE) newcob.val.Z      && $(MAKE) unpack-Z) || \
			(echo Fallback to sourceforge.net && \
			 $(MAKE) newcob.val.tar.gz && $(MAKE) unpack-gz);  \
		fi;  \
	fi
	@if ! test -f "newcob.val"; then \
		echo "newcob.val missing, automatic download and uncompress did not work"; \
		exit 1; \
	fi

clean-local:
	$(RM) copy copyalt $(MODULES_ALL) EXEC85$(EXEEXT) summary.log EXEC85.cob

$(MODULES_ALL): newcob.val EXEC85$(EXEEXT) $(srcdir)/EXEC85.conf.in Makefile.module.in
	@echo "Building module directory $@ ..."
	@mkdir -p ./$@
	@echo "*SELECT-MODULE `echo $@ | $(SED) 's/\(..\).*/\1/'`" > ./$@/EXEC85.conf
	@cat $(srcdir)/EXEC85.conf.in >> ./$@/EXEC85.conf
#	setting NEWCOB_VAL to the full name for EXEC85
	@if test -f "$(abs_builddir)/newcob.val"; then \
		export NEWCOB_VAL=$(abs_builddir)/newcob.val; \
	else \
		export NEWCOB_VAL=$(abs_srcdir)/newcob.val; \
	fi; \
	cd $@ && "$(abs_top_builddir)/pre-inst-env" ../EXEC85$(EXEEXT)
	@perl $(srcdir)/expand.pl $@/newcob.tmp $@
#	@$(RM) ./$@/newcob.tmp ./$@/newcob.log ./$@/EXEC85.conf
	@export CBL_LIST="`ls $@/*.CBL | cut -f2 -d/ | tr '\n' ' '`"  && \
	 $(SED) -e 's/##MODULE##/'"$@"'/'            \
	        -e 's|##COB85DIR##|'$(abs_srcdir)'|' \
	        -e 's|##DIFF_FLAGS##|'"$(DIFF_FLAGS)"'|' \
	        -e 's|##TESTS##|'"` echo $$CBL_LIST | $(SED) -e 's/\.CBL//g'`"'|' \
	        -e 's|##TESTS_LOCAL##|'"`echo $$CBL_LIST | $(SED) -e 's/\.CBL/-local/g'`"'|' \
	    $(srcdir)/Makefile.module.in > $@/Makefile
	@echo "Finished module directory $@."

EXEC85.cob: newcob.val
	@echo "Extracting EXEC85 program from newcob.val"
#	setting NEWCOB_VAL to the full name for the sed invocation
	@if test -f "$(abs_builddir)/newcob.val"; then \
		NEWCOB_VAL=$(abs_builddir)/newcob.val; \
	else \
		NEWCOB_VAL=$(abs_srcdir)/newcob.val; \
	fi; \
	$(SED) -e '/^\*END/,$$d'                                                        \
	       -e '1,/^\*HEADER/d'                                                      \
	       -e 's/^002500.*/           SELECT           POPULATION-FILE/'            \
	       -e 's/^002700.*/           "NEWCOB_VAL" ORGANIZATION LINE SEQUENTIAL./'  \
	       -e 's/^003000.*/           "newcob.tmp" ORGANIZATION LINE SEQUENTIAL./'  \
	       -e 's/^003100.*//'                                                       \
	       -e 's/^003400.*/           "unused"./'                                   \
	       -e 's/^003700.*/           "newcob.log"./'                               \
	       -e 's/^004000.*/           "EXEC85.conf" ORGANIZATION LINE SEQUENTIAL./' \
	    "$$NEWCOB_VAL" > EXEC85.cob || ($(RM) EXEC85.cob && false)


EXEC85$(EXEEXT): EXEC85.cob
	@echo "Compiling EXEC85 program"
	@if test -f "EXEC85.cob"; then EXEC_SRC="EXEC85.cob"; else EXEC_SRC="$(srcdir)/EXEC85.cob"; fi; \
	 "$(top_builddir)/pre-inst-env" cobc$(EXEEXT) $(COBC_FLAGS) -x "$$EXEC_SRC"
