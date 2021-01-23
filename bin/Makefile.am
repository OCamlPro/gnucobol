#
# Makefile gnucobolcpp/bin
#
# Copyright (C) 2001-2012, 2014, 2017 Free Software Foundation, Inc.
# Written by Roger While, Simon Sobisch, Sergey Kashyrin
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

bin_SCRIPTS = cob-config
bin_PROGRAMS = cobcrun
cobcrun_SOURCES = cobcrun.cpp
dist_man_MANS = cobcrun.1

AM_LDFLAGS = $(COB_EXPORT_DYN)
cobcrun_CXXFLAGS = $(AM_CXXFLAGS) -I$(top_srcdir) $(CODE_COVERAGE_CXXFLAGS)
cobcrun_LDADD = $(top_builddir)/lib/libsupport.a $(top_builddir)/libcob/libcob.la \
	$(CODE_COVERAGE_LIBS)

# Add rules for code-coverage testing, as provided AX_CODE_COVERAGE
@CODE_COVERAGE_RULES@
CODE_COVERAGE_BRANCH_COVERAGE = 1
CODE_COVERAGE_LCOV_OPTIONS = --no-external

cobcrun.1: cobcrun.cpp $(top_srcdir)/configure.ac
	$(MAKE) $(AM_MAKEFLAGS) cobcrun$(EXEEXT)
	help2man --output=cobcrun.1  --info-page=gnucobolcpp ./cobcrun$(EXEEXT)
