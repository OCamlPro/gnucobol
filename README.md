[GnuCOBOL](https://www.gnu.org/software/gnucobol/) is a free
COBOL compiler licensed under the GNU Public License (GPL).  
It implements a substantial part of the COBOL 85,
COBOL 2002 and COBOL 2014 standards, as well as many extensions
included in other COBOL compilers.

GnuCOBOL translates COBOL into C and compiles the translated code
using the native C compiler on various platforms, including Unix/Linux,
Mac OS X, and Microsoft Windows.

This package contains the following subdirectories:

*    cobc        COBOL compiler
*    libcob      COBOL run-time library
*    bin         COBOL driver program
*    build_aux   Helper scripts
*    lib         Helper routines for missing OS functionality
*    config      Configuration files
*    po          International messages
*    doc         'info' and PDF files
*    tests       Test suite (GnuCOBOL and framework for COBOL85)
*    extras      useful COBOL programs

All programs except those in lib and libcob are distributed under
the GNU General Public License.  See COPYING for details.

Programs in lib and libcob are distributed under the GNU Lesser
General Public License.  See COPYING.LESSER for details.

Requirements
============

GnuCOBOL uses other software packages, some of these are necessary,
some optional.  See DEPENDENCIES.md for a complete list.

If building GnuCOBOL from source, you can choose which optional
functionality to include via the configure script.  When using
pre-built binaries -- for example, via your OS package 
manager -- the packager will have made those choices for you.  

Building from source
====================

GnuCOBOL is built and installed using the `configure` script generated
by GNU autotools.  If you're unfamiliar with `configure`, see the
INSTALL file for detailed information about it.  Special requirements
and further installation notes are listed below.

We recommend creating a build directory; these instructions assume
your build directory is `build`, a sub-directory of the source tree
root, and that that is your current working directory.  These
instructions redirect the configuration and compilation messages to a
file, partly to avoid a slew of messages scrolling off the screen, and
to make any errors more prominent.

To see the list of configuration options, use `../configure --help`. 

By default, the configured PREFIX is `/usr/local`.  That means the
binaries will be installed to `/usr/local/bin`, libraries to
`/usr/local/lib`, and so on.  The PREFIX may be changed by specifying
`--prefix=<dir>` configure option.  Other options control other 
install directories.  

Once you've decided on your options, generate the build tree with

*  `../configure [options] > log` 

and build the compiler and run-time library with 

*  `make > log`

Tests
=====

To verify GnuCOBOL works before installing it, run the internal
testsuite. Simply do

*  `make check > log`

This MUST succeed.  Please report any failures.

Also included are COBOL85 standard tests provided by the NIST.  
They are run with

* `make test > log`

If the the COBOL85 testsuite is not already in the build- or source-tree,
`make test` will download it.  For details see tests/cobol85/README.

** NOTE **
   The language interpreter "perl" is required to run COBOL85 tests.


 If you want to run both testsuites you can run
 
 *  `make checkall`

Installation
============

To install GnuCOBOL, including the compiler, run-time library, and
documentation, 
 
 * `make install > log`

Unless you have taken steps to ensure you can create files in the
PREFIX directory, you'll probably need super-user privileges for this step.  


Un-installation
===============

You can get back to a clean installation status by running

*  `make distclean`

ISAM Support
============

The following is only interesting for advanced use.  

The COBOL language supports the use of ISAM files.  Unlike mainframe
operating systems, those that GnuCOBOL runs on do not include any
record-oriented system services. GnuCOBOL can either be built without
ISAM support, or with it, in which case the support may by provided by
one of several libraries.  The choice is expressed as a `configure`
option.

To exclude ISAM support, use 

*  `--without-db` You will not be able to use indexed I/O

By default, ISAM support is provide by Berkeley DB. You can say so
explicitly with `--with-db`.  Or you can select from one of the
following options,

*  `--with-cisam` to use CISAM
*  `--with-disam` to use DISAM
*  `--with-vbisam` to use VBISAM

Development
===========

If you wish to hack the GnuCOBOL source or build from version control,
see HACKING.

Further information about the project, including the source code repository,
history, and frequently asked questions, may be found at 

*  https://www.gnu.org/software/gnucobol/
*  https://sourceforge.net/projects/gnucobol
*  https://savannah.gnu.org/projects/gnucobol

