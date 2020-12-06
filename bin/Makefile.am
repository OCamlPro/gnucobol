#
# Makefile gnucobol/bin
#
# Copyright (C) 2001-2012, 2014, 2017-2020 Free Software Foundation, Inc.
# Written by Roger While, Simon Sobisch
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

bin_SCRIPTS = cob-config
bin_PROGRAMS = cobcrun
cobcrun_SOURCES = cobcrun.c
dist_man_MANS = cobcrun.1 cob-config.1
COBCRUN = cobcrun$(EXEEXT)

# AM_LDFLAGS = $(COB_EXPORT_DYN)
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)/lib -I$(top_srcdir)/lib
AM_CFLAGS = $(CODE_COVERAGE_CFLAGS)
cobcrun_LDADD = $(top_builddir)/libcob/libcob.la \
	$(top_builddir)/lib/libsupport.la \
	$(PROGRAMS_LIBS) $(CODE_COVERAGE_LIBS)

# Add rules for code-coverage testing, as provided AX_CODE_COVERAGE
include $(top_srcdir)/aminclude_static.am

clean-local: code-coverage-clean
dist-clean-local: code-coverage-dist-clean

CODE_COVERAGE_BRANCH_COVERAGE=1
CODE_COVERAGE_LCOV_OPTIONS =  --no-external

MAINTAINERCLEANFILES = cobcrun.1

HELPSOURCES_COBCRUN = cobcrun.c $(top_srcdir)/configure.ac
HELPSOURCES_COBCONFIG = cob-config.in $(top_srcdir)/configure.ac
HELP2MAN_OPTS = --info-page=$(PACKAGE)

if MAKE_HAS_PREREQ_ONLY
cobcrun.1: $(HELPSOURCES_COBCRUN) | $(COBCRUN)
	"$(top_builddir)/pre-inst-env" $(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) $(COBCRUN)
else
cobcrun.1: $(HELPSOURCES_COBCRUN)
	"$(top_builddir)/pre-inst-env" $(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) $(COBCRUN)
endif

if MAKE_HAS_PREREQ_ONLY
cob-config.1: $(HELPSOURCES_COBCONFIG) | ./cob-config
	$(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) ./cob-config
else
	$(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) ./cob-config
endif
