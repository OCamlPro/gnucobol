#
# Makefile gnucobol/libcob
#
# Copyright (C) 2003-2012, 2014, 2017-2020, 2022-2025 Free Software Foundation, Inc.
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

lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c move.c numeric.c strings.c \
	fileio.c call.c intrinsic.c termio.c screenio.c reportio.c cobgetopt.c \
	mlio.c coblocal.h cconv.c system.def profiling.c

EXTRA_DIST = xmlevent.def

if LOCAL_CJSON
nodist_libcob_la_SOURCES = cJSON.c
DISTCLEANFILES = cJSON.c cJSON.h
endif

#ToDo Man Page
#dist_man_MANS = libcob.3
#COBCRUN = cobcrun$(EXEEXT)

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)/lib -I$(top_srcdir)/lib \
	$(LIBCOB_CPPFLAGS) -DLOCALEDIR=\"$(localedir)\"
AM_CFLAGS = $(CODE_COVERAGE_CFLAGS)
# note: currently misses libsupport...
libcob_la_LIBADD = $(LIBCOB_LIBS) $(CODE_COVERAGE_LIBS)
libcob_la_LDFLAGS = $(COB_FIX_LIBTOOL) -version-info 6:0:2 -no-undefined
AM_LDFLAGS = $(COB_FIX_LIB)

pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h version.h cobgetopt.h \
	exception.def exception-io.def statement.def

# Add rules for code-coverage testing, as provided by AX_CODE_COVERAGE
include $(top_srcdir)/aminclude_static.am

clean-local: code-coverage-clean
dist-clean-local: code-coverage-dist-clean

CODE_COVERAGE_BRANCH_COVERAGE=1
CODE_COVERAGE_LCOV_OPTIONS =  --no-external

#HELPSOURCES = $(top_srcdir)/bin/cobcrun.c $(top_srcdir)/configure.ac
#HELP2MAN_OPTS = --info-page=$(PACKAGE) -n "GnuCOBOL runtime library and API functions" -h "-special -flag"
#if MAKE_HAS_PREREQ_ONLY
#libcob.3: $(HELPSOURCES) | $(COBCRUN)
#	"$(top_builddir)/pre-inst-env" $(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) $(COBCRUN)
#else
#libcob.3: $(HELPSOURCES)
#	"$(top_builddir)/pre-inst-env" $(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) $(COBCRUN)
#endif
