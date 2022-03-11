#
# Makefile gnucobol
#
# Copyright (C) 2003-2012, 2014-2020, 2022 Free Software Foundation, Inc.
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

include_HEADERS = libcob.h

SUBDIRS = . lib libcob bin cobc config copy po extras doc tests

bin: libcob
cobc: libcob
libcob: lib tarstamp.h
extras: cobc
doc po tests: cobc bin

ACLOCAL_AMFLAGS = -I m4 --install
DISTCLEANFILES = $(BUILT_SOURCES)
# CLEANFILES = $(bin_SCRIPTS)
dist_noinst_SCRIPTS = autogen.sh po/update_linguas.sh \
	build_aux/bootstrap \
	build_aux/create_win_dist.sh build_aux/create_mingw_bindist.sh

# wrapper for the build environment
noinst_SCRIPTS = pre-inst-env

EXTRA_DIST = gnucobol.spec DEPENDENCIES HACKING

# add rules for code-coverage testing, as defined by AX_CODE_COVERAGE
include $(top_srcdir)/aminclude_static.am

clean-local: code-coverage-clean
dist-clean-local: code-coverage-dist-clean

CODE_COVERAGE_BRANCH_COVERAGE=1

# files shipped with the package that should be 755'ed:
FILES_TO_BE_EXECUTABLE = $(dist_noinst_SCRIPTS) \
	configure tests/testsuite \
	build_aux/config.guess build_aux/config.sub build_aux/config.rpath \
	build_aux/depcomp build_aux/install-sh build_aux/ltmain.sh build_aux/mdate-sh \
	build_aux/missing build_aux/mkinstalldirs build_aux/ylwrap

tarstamps = $(top_distdir)/tarstamp.h tarstamp.h

# all parts but tarstamp.h should not be necessary...
dist-hook:
	rm -f $(top_distdir)/tarstamp.h
	$(MAKE) $(AM_MAKEFLAGS) $(top_distdir)/tarstamp.h
# fix access to cater for bad version control use / copy / read-only file system
	find $(top_distdir) -type d -print | xargs chmod 755	# otherwise directories have 777
	find $(top_distdir) -type f -print | xargs chmod 644	# otherwise files are unchanged but with u+r
# because of the global file change, adjust here again
	cd $(top_distdir) && chmod 755 $(FILES_TO_BE_EXECUTABLE)
# fix timestamps to cater for bad version control use / copy
	touch $(top_distdir)/m4/*.m4
	touch $(top_distdir)/aclocal.m4
	touch $(top_distdir)/Makefile.in
	touch $(top_distdir)/*/Makefile.in
	touch $(top_distdir)/*/*/Makefile.in
	touch $(top_distdir)/configure
	touch $(top_distdir)/config.h.in
	touch $(top_distdir)/doc/stamp-vti
#	touch $(top_distdir)/cobc/ppparse.c
#	touch $(top_distdir)/cobc/parser.c
#	touch $(top_distdir)/cobc/pplex.c
#	touch $(top_distdir)/cobc/scanner.c
#	$(top_distdir)/doc/cobcinfo.sh "fixtimestamps"
#	touch $(top_distdir)/libcob/libcob.3
#	touch $(top_distdir)/bin/cobcrun.1
#	touch $(top_distdir)/cobc/cobc.1

# Create dist_win manually (dist-zip would have the same content as dist-gzip)
distwindir = $(distdir)_win
distwindir: distdir $(top_srcdir)/build_windows $(top_srcdir)/build_aux/create_win_dist.sh
	EXTSRCDIR=$(abs_srcdir) EXTDISTDIR=$(distdir) EXTWINDISTDIR=$(distwindir) $(top_srcdir)/build_aux/create_win_dist.sh

distwin-zip: distwindir
	rm -f "$(distwindir).zip"
	zip -rq "$(distwindir).zip" "$(distwindir)"

distmingwdir = GnuCOBOL_mingw
distmingwdir: all $(top_srcdir)/build_aux/create_mingw_bindist.sh
	EXTSRCDIR=$(abs_srcdir) EXTDISTDIR=$(distmingwdir) EXTBUILDDIR=$(abs_builddir) $(top_srcdir)/build_aux/create_mingw_bindist.sh

distwin:   distwindir
distmingw: distmingwdir

distbindir = $(distdir)_bin
distbindir: all distdir
	$(MAKE) $(AM_MAKEFLAGS) DESTDIR=$(abs_builddir)/$(distbindir) install
	install -d "$(distbindir)/${datarootdir}/licenses/$(PACKAGE)"
	install -p -m644 $(distdir)/COPYING*          "$(distbindir)/${datarootdir}/licenses/$(PACKAGE)"
	install -d "$(distbindir)/${datarootdir}/doc/$(PACKAGE)"
	install -p -m644 $(distdir)/README*           "$(distbindir)/${datarootdir}/doc/$(PACKAGE)"
	install -p -m644 $(distdir)/NEWS*             "$(distbindir)/${datarootdir}/doc/$(PACKAGE)"
	install -p -m644 $(distdir)/ChangeLog         "$(distbindir)/${datarootdir}/doc/$(PACKAGE)"
	install -p -m644 $(distdir)/bin/ChangeLog     "$(distbindir)/${datarootdir}/doc/$(PACKAGE)/ChangeLog_bin"
	install -p -m644 $(distdir)/cobc/ChangeLog    "$(distbindir)/${datarootdir}/doc/$(PACKAGE)/ChangeLog_cobc"
	install -p -m644 $(distdir)/libcob/ChangeLog  "$(distbindir)/${datarootdir}/doc/$(PACKAGE)/ChangeLog_libcob"
	install -p -m644 $(distdir)/doc/gnucobol*.pdf "$(distbindir)/${datarootdir}/doc/$(PACKAGE)"

DISTBIN_TARGETS = distbin-gzip
gc__remove_bindistdir = rm -rf $(distbindir)
gc__post_remove_bindistdir = $(gc__remove_bindistdir)
distbin-gzip: distbindir
	tardir=$(distbindir) && $(am__tar) | eval GZIP= gzip $(GZIP_ENV) -c >$(distbindir).tar.gz
	$(gc__post_remove_bindistdir)
	$(am__post_remove_distdir)
distbin-bzip2: distbindir
	tardir=$(distbindir) && $(am__tar) | BZIP2=$${BZIP2--9} bzip2 -c >$(distbindir).tar.bz2
	$(gc__post_remove_bindistdir)
	$(am__post_remove_distdir)
distbin-lzip: distbindir
	tardir=$(distbindir) && $(am__tar) | lzip -c $${LZIP_OPT--9} >$(distbindir).tar.lz
	$(gc__post_remove_bindistdir)
	$(am__post_remove_distdir)
distbin-xz: distbindir
	tardir=$(distbindir) && $(am__tar) | XZ_OPT=$${XZ_OPT--e} xz -c >$(distbindir).tar.xz
	$(gc__post_remove_bindistdir)
	$(am__post_remove_distdir)

distbin:
	$(MAKE) $(AM_MAKEFLAGS) $(DISTBIN_TARGETS) gc__post_remove_bindistdir='@:' am__post_remove_distdir='@:'
	$(gc__post_remove_bindistdir)
	$(am__post_remove_distdir)

$(tarstamps):
	@echo "Creating $@..."
	@{								\
	  echo "#define COB_TAR_DATE    \"`LC_ALL=C date -u +'%b %d %Y %T'` UTC\""; \
	  echo "#define COB_NUM_TAR_DATE ` LC_ALL=C date -u +'%Y%m%d'`"; \
	  echo "#define COB_NUM_TAR_TIME ` LC_ALL=C date -u +'%H%M%S'`"; \
	} > $@

# TODO: possibly disable / adjust depending on what
#       configure.ac founds (currently not checked there)
vcs-update:
	svn update $(srcdir)


# targets that are only logical targets and should always be executed
.PHONY: test checkall checkmanual vcs-update distwin distmingw \
	distbin distbin-gzip distbin-bzip2 distbin-lzip distbin-xz

test: all
	cd tests && $(MAKE) $(AM_MAKEFLAGS) test
checkmanual: all
	cd tests && $(MAKE) $(AM_MAKEFLAGS) checkmanual

checkall: check test

