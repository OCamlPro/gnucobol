#
# Makefile gnucobol/extras
#
# Copyright (C) 2012, 2014, 2017-2020 Free Software Foundation, Inc.
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

extrasdir = @COB_LIBRARY_PATH@
if COB_MAKE_RUN_BINARIES
extras_DATA = CBL_OC_DUMP.$(COB_MODULE_EXT)
endif
COBC = cobc$(EXEEXT)

EXTRA_DIST = CBL_OC_DUMP.cob
CLEANFILES = $(extras_DATA)

SUFFIXES = .cob .$(COB_MODULE_EXT)
.cob.$(COB_MODULE_EXT):
	("$(top_builddir)/pre-inst-env" $(COBC) -m -Wall -O2 -o "$@" "$<" || \
	 "$(top_builddir)/pre-inst-env" $(COBC) -m -Wall     -o "$@" "$<" || \
	 "$(top_builddir)/pre-inst-env" $(COBC) -m -Wall -vv -o "$@" "$<")
