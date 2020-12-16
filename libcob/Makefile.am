#
# Makefile gnucobol/libcob
#
# Copyright (C) 2003-2012, 2014, 2017-2020 Free Software Foundation, Inc.
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

if COB_MAKE_CISAM_LIB
lib_ci = libcobci.la
libcobci_la_LIBADD = $(LIBCOB_CISAM)
else
lib_ci =
endif

if COB_MAKE_DISAM_LIB
lib_di = libcobdi.la
libcobdi_la_LIBADD = $(LIBCOB_DISAM)
else
lib_di =
endif

if COB_MAKE_VBISAM_LIB
lib_vb = libcobvb.la
libcobvb_la_LIBADD = $(LIBCOB_VBISAM)
else
lib_vb =
endif

if COB_MAKE_VBCISAM_LIB
lib_vc = libcobvc.la
libcobvc_la_LIBADD = $(LIBCOB_VBCISAM)
else
lib_vc =
endif

lib_LTLIBRARIES = libcob.la $(lib_ci) $(lib_di) $(lib_vb) $(lib_vc)
libcob_la_SOURCES = common.c move.c numeric.c strings.c \
	fileio.c fextfh.c fisam.c fbdb.c focextfh.c flmdb.c fodbc.c foci.c fsqlxfd.c \
	call.c intrinsic.c termio.c screenio.c reportio.c cobgetopt.c \
	mlio.c


#ToDo Man Page
#dist_man_MANS = libcob.3
#COBCRUN = cobcrun$(EXEEXT)

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)/lib -I$(top_srcdir)/lib \
	$(LIBCOB_CPPFLAGS) -DLOCALEDIR=\"$(localedir)\"
AM_CFLAGS = $(CODE_COVERAGE_CFLAGS)
libcob_la_LIBADD = $(LIBCOB_LIBS) $(top_builddir)/lib/libsupport.a \
	$(CODE_COVERAGE_LIBS)
libcob_la_LDFLAGS = $(COB_FIX_LIBTOOL) -version-info 5:0:0 -no-undefined
AM_LDFLAGS = $(COB_FIX_LIB)

EXTRA_DIST = coblocal.h cobinternal.h system.def sysdefines.h fileio.h
pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h cobgetopt.h exception.def

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
