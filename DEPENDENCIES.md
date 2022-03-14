GnuCOBOL uses other software packages, some of these are necessary,
some optional. You find a detailed list of software below.

These packages provide **compile-time** and **run-time** support. That is,
they include C header files that are used during GnuCOBOL's C
compilation phase, and dynamic libraries linked either to `libcob`, or
to the compiled binary (the compiled version of your COBOL
program). Packaging systems, particularly on Linux, sometimes include
only the run-time components in the main package, and split the
compile-time pieces off to a "development" package.

You need both. Header files are required for compilation, and
libraries for run-time support.

If your operating system includes a package management system, we
suggest you use it, rather than building the dependencies from source,
unless you have specific needs.

Definitely Required
===================

GnuCOBOL requires *one of* the following external libraries to be installed
for decimal arithmetic:

* [GNU MP](https://gmplib.org) (libgmp) 4.1.2 or later.
* [MPIR](http://mpir.org) (libgmp - MPIR gmp-compat) 1.3.1 or later.
This is preferred when compiling on Windows with other compilers than gcc.

GNU MP and MPIR are distributed under GNU Lesser General Public License.

Seldom Required
===============

GnuCOBOL requires support for dynamic linking, a feature of nearly all
target operating systems. Among those that include it are Windows,
Solaris, Linux, any BSD, and recent versions of AIX (>= 5.1) and HP-UX
(>= 11.1).

If your OS is some flavor of Unix or DOS and has no support for **dlopen**(3),
it can be provided by

* [GNU Libtool](https://www.gnu.org/software/libtool/libtool.html)  (libltdl)

`libltdl` is used to implement dynamic CALL statements on those systems.

GNU Libtool is distributed under GNU Lesser General Public License.

Not Required
============
(but helpful)

All dependencies discussed from this point forward support optional
features of GnuCOBOL.

ISAM Support
------------

Support for Indexed-Sequential file I/O (ISAM):  By default, absent other
`configure` options, ISAM support comes from Berkeley DB,
but that's only one alternative.  
Choose *one* of the following: 

*   [Berkeley DB](https://www.oracle.com/) (libdb) 4.1 or later
    https://www.oracle.com/technology/products/berkeley-db/db/index.html

    Berkeley DB is distributed under Oracles own open-source license.
    Note that if you linked your software with Berkeley DB,
    you must distribute the source code of your software along with your
    software, or you have to pay royalty to Oracle.

*   [VBISAM](https://sourceforge.net/projects/vbisam/) (libvbisam) 2.0 or later

    VBISAM is distributed under GNU Lesser General Public License.

*   [DISAM](http://www.isamcentral.com) (libdisam)

    DISAM is distributed under the proprietary License
    "Byte Designs Ltd. DISAM Software License".


SCREEN SECTION
--------------

Support for SCREEN SECTION and/or extended ACCEPT/DISPLAY is provided
by *one* of the following:

*   [Ncurses](https://www.gnu.org/software/ncurses/ncurses.html)
    (ncurses or ncursesw) 5.2 or later
    
    Ncurses is distributed under a BSD style license.

*   [PDCurses](https://pdcurses.org/) or
    [PDCursesMod](https://github.com/Bill-Gray/PDCursesMod/),
    especially for MinGW and native windows ports

    PDCurses is distributed as Public Domain.

*   Unix curses (if supplied by your OS)

XML Support
-----------

Support for GENERATE XML is provided by:

*   [libxml2](http://xmlsoft.org)

    libxml2 is distributed under MIT License.

JSON support
------------

Support for GENERATE JSON is provided by *one* of the following:

*   [cJSON](https://github.com/DaveGamble/cJSON) >= 1.3.0 - 

    cJSON is distributed under MIT License.

*   [JSON-C](https://github.com/json-c/json-c) >= 1.12 - 

    JSON-C is distributed under Expat License.

