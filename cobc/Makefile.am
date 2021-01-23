#
# Makefile gnucobolcpp/cobc
#
# Copyright (C) 2003-2012, 2014-2017 Free Software Foundation, Inc.
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

bin_PROGRAMS = cobc
cobc_SOURCES = cobc.cpp cobc.h config.cpp tree.cpp tree.h reserved.cpp error.cpp \
	parser.cpp parser.hpp scanner.cpp field.cpp typeck.cpp codegen.cpp ppparse.cpp \
	ppparse.hpp pplex.cpp config.def flag.def warning.def codeoptim.def \
	ppparse.def codeoptim.cpp naming.cpp utils.cpp vcache.h

cobc_CXXFLAGS = -I$(top_srcdir) $(CODE_COVERAGE_CXXFLAGS)
cobc_LDADD = $(COBC_LIBS) $(top_builddir)/libcob/libcob.la $(top_builddir)/lib/libsupport.a \
	$(CODE_COVERAGE_LIBS)

dist_man_MANS = cobc.1


BUILT_SOURCES = parser.cpp parser.hpp scanner.cpp ppparse.cpp ppparse.hpp pplex.cpp
EXTRA_DIST = ppparse.y pplex.l parser.y scanner.l
CLEANFILES = parser.output ppparse.output

# Add rules for code-coverage testing, as provided by AX_CODE_COVERAGE
@CODE_COVERAGE_RULES@
CODE_COVERAGE_BRANCH_COVERAGE=1
CODE_COVERAGE_LCOV_OPTIONS =  --no-external

cobc.1: cobc.cpp config.def flag.def warning.def $(top_srcdir)/configure.ac
	$(MAKE) $(AM_MAKEFLAGS) cobc$(EXEEXT)
	help2man --output=cobc.1  --info-page=gnucobolcpp ./cobc$(EXEEXT)

.l.cpp:
	LC_CTYPE=C flex -s -o$@ $<

.y.cpp:
	bison -o $@ $<
