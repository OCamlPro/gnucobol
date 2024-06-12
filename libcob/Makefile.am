#
# Makefile gnucobol/libcob
#
# Copyright (C) 2003-2012, 2014, 2017-2023 Free Software Foundation, Inc.
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
	java.c \
	mlio.c coblocal.h cconv.c system.def profiling.c

if LOCAL_CJSON
nodist_libcob_la_SOURCES = cJSON.c
DISTCLEANFILES = cJSON.c cJSON.h
endif

#ToDo Man Page
#dist_man_MANS = libcob.3
#COBCRUN = cobcrun$(EXEEXT)

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)/lib -I$(top_srcdir)/lib \
	$(LIBCOB_CPPFLAGS)
AM_CFLAGS = $(CODE_COVERAGE_CFLAGS)
AM_LDFLAGS = $(COB_FIX_LIB) $(COB_FIX_LIBTOOL) -no-undefined


libcob_la_SOURCES = common.c move.c numeric.c strings.c \
	fileio.c fsqlxfd.c fextfh.c focextfh.c  mlio.c \
	call.c cobcapi.c intrinsic.c termio.c screenio.c reportio.c cobgetopt.c \
	coblocal.h cobinternal.h system.def sysdefines.h fileio.h

# note: currently misses libsupport...
libcob_la_LIBADD = $(LIBCOB_LIBS) $(CODE_COVERAGE_LIBS)
libcob_la_LDFLAGS = $(AM_LDFLAGS) -version-info $(LIBCOB_VER):0:0

INDEXED_LD_FLAGS = -version-info 1:0:0

if COB_MAKE_CISAM_LIB
lib_ci = libcobci.la
libcobci_la_SOURCES = libcobci.c fsqlxfd.c  # includes fisam.c
libcobci_la_LIBADD = libcob.la $(CISAM_LIBS)
libcobci_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcobci_la_CFLAGS = $(AM_CFLAGS) $(CISAM_CFLAGS)
else
lib_ci =
endif

if COB_MAKE_DISAM_LIB
lib_di = libcobdi.la
libcobdi_la_SOURCES = libcobdi.c fsqlxfd.c  # includes fisam.c
libcobdi_la_LIBADD = libcob.la $(DISAM_LIBS)
libcobdi_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcobdi_la_CFLAGS = $(AM_CFLAGS) $(DISAM_CFLAGS)
else
lib_di =
endif

if COB_MAKE_VBISAM_LIB
lib_vb = libcobvb.la
libcobvb_la_SOURCES = libcobvb.c fsqlxfd.c  # includes fisam.c
libcobvb_la_LIBADD = libcob.la $(VBISAM_LIBS)
libcobvb_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcobvb_la_CFLAGS = $(AM_CFLAGS) $(VBISAM_CFLAGS)
else
lib_vb =
endif

if COB_MAKE_VISAM_LIB
lib_vc = libcobvc.la
libcobvc_la_SOURCES = libcobvc.c fsqlxfd.c  # includes fisam.c
libcobvc_la_LIBADD = libcob.la $(VISAM_LIBS)
libcobvc_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcobvc_la_CFLAGS = $(AM_CFLAGS) $(VISAM_CFLAGS)
else
lib_vc =
endif

if COB_MAKE_ODBC_LIB
lib_od = libcobod.la
libcobod_la_LIBADD = libcob.la $(ODBC_LIBS)
libcobod_la_SOURCES = fodbc.c fsqlxfd.c
libcobod_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcobod_la_CFLAGS = $(AM_CFLAGS) $(ODBC_CFLAGS)
else
lib_od =
endif

if COB_MAKE_OCI_LIB
lib_oc = libcoboc.la
libcoboc_la_LIBADD = libcob.la $(OCI_LIBS) 
libcoboc_la_SOURCES = foci.c fsqlxfd.c
libcoboc_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcoboc_la_CFLAGS = $(AM_CFLAGS) $(OCI_CFLAGS)
else
lib_oc =
endif

if COB_MAKE_BDB_LIB
lib_db = libcobdb.la
libcobdb_la_LIBADD = libcob.la $(BDB_LIBS) 
libcobdb_la_SOURCES = fbdb.c fsqlxfd.c
libcobdb_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcobdb_la_CFLAGS = $(AM_CFLAGS) $(BDB_CFLAGS)
else
lib_db =
endif

if COB_MAKE_LMDB_LIB
lib_lm = libcoblm.la
libcoblm_la_LIBADD = libcob.la $(LMDB_LIBS) 
libcoblm_la_SOURCES = flmdb.c fsqlxfd.c
libcoblm_la_LDFLAGS = $(AM_LDFLAGS) $(INDEXED_LD_FLAGS)
libcoblm_la_CFLAGS = $(AM_CFLAGS) $(LMDB_CFLAGS)
else
lib_lm =
endif

lib_LTLIBRARIES = libcob.la $(lib_ci) $(lib_di) $(lib_vb) $(lib_vc) \
	$(lib_od) $(lib_oc) $(lib_db) $(lib_lm)

EXTRA_DIST = fisam.c
pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h version.h cobgetopt.h cobcapi.h \
	exception.def exception-io.def statement.def

# Add rules for code-coverage testing, as provided by AX_CODE_COVERAGE
include $(top_srcdir)/aminclude_static.am

clean-local: code-coverage-clean
dist-clean-local: code-coverage-dist-clean

CODE_COVERAGE_BRANCH_COVERAGE=1
CODE_COVERAGE_LCOV_OPTIONS =  --no-external

#HELPSOURCES = $(top_srcdir)/bin/cobcrun.c $(top_srcdir)/configure.ac
#HELP2MAN_OPTS = --info-page=$(PACKAGE) -specialflag
#if MAKE_HAS_PREREQ_ONLY
#libcob.3: $(HELPSOURCES) | $(COBCRUN)
#	"$(top_builddir)/pre-inst-env" $(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) $(COBCRUN)
#else
#libcob.3: $(HELPSOURCES)
#	"$(top_builddir)/pre-inst-env" $(HELP2MAN) --output=$@ $(HELP2MAN_OPTS) $(COBCRUN)
#endif
