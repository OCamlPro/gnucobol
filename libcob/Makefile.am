#
# Makefile gnucobolcpp/libcob
#
# Copyright (C) 2003-2012, 2014, 2017 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Simon Sobisch, Sergey Kashyrin
#
# This file is part of GnuCOBOL C++.
#
# The GnuCOBOL C++ compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GnuCOBOL C++ is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuCOBOL C++.  If not, see <http://www.gnu.org/licenses/>.

lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.cpp move.cpp numeric.cpp strings.cpp cobdecimal.cpp \
	fileio.cpp call.cpp intrinsic.cpp termio.cpp screenio.cpp btmsadd.cpp \
	cobgetopt.cpp

libcob_la_CXXFLAGS = $(AM_CFLAGS) -I$(top_srcdir) $(CODE_COVERAGE_CXXFLAGS)
libcob_la_LIBADD = $(LIBCOB_LIBS) $(CODE_COVERAGE_LIBS)
libcob_la_LDFLAGS = $(COB_FIX_LIBTOOL) -version-info 4:0:0 -no-undefined
AM_LDFLAGS = $(COB_FIX_LIB)

EXTRA_DIST = coblocal.h system.def
pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h cobdecimal.h cobgetopt.h exception.def

# Add rules for code-coverage testing, as provided by AX_CODE_COVERAGE
@CODE_COVERAGE_RULES@
CODE_COVERAGE_BRANCH_COVERAGE=1
CODE_COVERAGE_LCOV_OPTIONS =  --no-external
