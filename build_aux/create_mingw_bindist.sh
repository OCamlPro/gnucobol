#!/bin/bash
# create_mingw_bindist.sh gnucobol
#
# Copyright (C) 2016-2021 Free Software Foundation, Inc.
# Written by Simon Sobisch
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


# This shell script needs to be sourced from Makefile processing,
# otherwise set EXTSRCDIR and EXTBUILDDIR before calling this script
# AND make sure EXTBUILDDIR exists with the right content.

# Check we're in a MinGW environment
if test ! -d "/mingw/bin"; then
	echo "binary mingw dist packages can only be created from MSYS/MinGW"
	echo "directory /mingw/bin not found"
	exit 99
fi

# Check necessary vars:
if test "x$EXTBUILDDIR" = "x"; then
	EXTBUILDDIR="."
	echo "EXTBUILDDIR" not set, "." assumed
fi
if test "x$EXTSRCDIR" = "x"; then
	EXTSRCDIR="."
	echo "EXTSRCDIR" not set, "." assumed
fi
if test ! -f "$EXTBUILDDIR/config.log"; then
	echo "$EXTBUILDDIR/config.log" does not exist, aborting "$0"
	exit 4
fi
if test ! -f "$EXTSRCDIR/configure"; then
	echo "$EXTSRCDIR/configure" does not exist, aborting "$0"
	exit 5
fi
if test "x$EXTDISTDIR" != "x"; then
	target_dir="$EXTDISTDIR"
else
	target_dir="$EXTBUILDDIR/GnuCOBOL_mingw"
	echo "EXTDISTDIR" not set, "$EXTBUILDDIR/GnuCOBOL_mingw" assumed
fi

# getting version information, testing the current build works
versinfo=$($EXTBUILDDIR/pre-inst-env cobcrun -v --version | tail -n2)
versinfo_cmds=$(echo "echo. $(echo "$versinfo" | sed -e 's/^/\&\& echo /')" | tr '\n' ' ') 

# Create folder
echo
echo "Building MinGW dist package for GnuCOBOL containing"
echo "$versinfo"
echo
echo "target: $target_dir"
if test -e "$target_dir"; then
	echo "target directory already exist, aborting" && exit 95
	# fdate=$(date +%F-%h%m%S)
	# echo "target directory already exist - renaming it to $target_dir-$fdate"
	# mv "$target_dir" "$target_dir-$fdate"
	# if test -e "$target_dir"; then
	# 	echo "cannot move old target directory" && exit 98
	# fi
fi
mkdir "$target_dir" || (echo "cannot create target directory" && exit 97)
pushd "$target_dir" 1>/dev/null
if test "$target_dir" != "$(pwd)"; then
   target_dir="$(pwd)"
	echo "target (resolved): $target_dir"
fi
popd 1>/dev/null

echo && echo copying MinGW files...
echo "  bin..."
cp -pr "/mingw/bin"          "$target_dir/"
echo "  include..."
cp -pr "/mingw/include"      "$target_dir/"
echo "  lib..."
cp -pr "/mingw/lib"          "$target_dir/"
echo "  libexec..."
cp -pr "/mingw/libexec"      "$target_dir/"
echo "  share/locale..."
# note: possible copying more of share later
cp -pr "/mingw/share/locale" "$target_dir/"

echo && echo copying GnuCOBOL files...
cp -pr "$EXTBUILDDIR/extras" "$target_dir/"
cp -pr "$EXTSRCDIR/copy"     "$target_dir/"
cp -pr "$EXTSRCDIR/config"   "$target_dir/"

cp -p "$EXTBUILDDIR/cobc/.libs/cobc.exe" "$target_dir/bin/"
cp -p "$EXTBUILDDIR/bin/.libs/cobcrun.exe" "$target_dir/bin/"
cp -p $EXTBUILDDIR/libcob/.libs/libcob*.dll  "$target_dir/bin/"
cp -p $EXTBUILDDIR/libcob/.libs/libcob.*  "$target_dir/lib/"
mkdir "$target_dir/include/libcob"
cp -p $EXTSRCDIR/libcob.h    "$target_dir/include/"
cp -p $EXTSRCDIR/libcob/common.h  "$target_dir/include/libcob"
cp -p $EXTSRCDIR/libcob/cobcapi.h  "$target_dir/include/libcob"
cp -p $EXTSRCDIR/libcob/exception*.def  "$target_dir/include/libcob"

echo && echo copying docs...

pushd "$EXTSRCDIR" 1>/dev/null

for file in             \
	"ChangeLog"         \
	"NEWS"              \
	"THANKS"            \
	"COPYING"           \
	"COPYING.LESSER"    \
	"COPYING.DOC"    \
	"COPYING.DOC"
do
   sed -e 's/\r*$/\r/' "$file" > "$target_dir/$(basename "$file").txt"
done
sed -e 's/\r*$/\r/' "bin/ChangeLog" > "$target_dir/ChangeLog_bin.txt"
sed -e 's/\r*$/\r/' "cobc/ChangeLog" > "$target_dir/ChangeLog_cobc.txt"
sed -e 's/\r*$/\r/' "libcob/ChangeLog" > "$target_dir/ChangeLog_libcob.txt"

# copy manpages (checkme) ...
#cp bin/cobcrun.1
#cp cobc/cobc.1
##cp libcob/libcob.3
# ... and locales

echo && echo installing locales...
make -C "$EXTBUILDDIR/po" install-data-yes localedir="$target_dir/locale"

popd 1>/dev/null

pushd "$EXTBUILDDIR" 1>/dev/null
sed -e 's/\r*$/\r/' "config.log" > "$target_dir/config.log"
if test -f "tests/testsuite.log"; then
	sed -e 's/\r*$/\r/' "tests/testsuite.log" > "$target_dir/testsuite.log"
else
	echo "WARNING: GnuCOBOL testsuite results not found!"
fi
if test -f "tests/cobol85/summary.log"; then
	sed -e 's/\r*$/\r/' "tests/cobol85/summary.log" > "$target_dir/NIST_summary.log"
else
	echo "WARNING: NIST results not found!"
fi

if test -f "doc/gnucobol.pdf"; then
	cp -p "doc/gnucobol.pdf"   "$target_dir/GnuCOBOL.pdf"
else
	if test -f "$EXTSRCDIR/doc/gnucobol.pdf"; then
		cp -p "$EXTSRCDIR/doc/gnucobol.pdf"   "$target_dir/GnuCOBOL.pdf"
	else
		echo "WARNING: GnuCOBOL.pdf will be missing"
	fi
fi
popd 1>/dev/null

cat > "$target_dir/set_env.cmd" << _FEOF
@echo off

:: Check if called already
:: if yes, check if called from here - exit, in any other case 
:: raise warning and reset env vars
if not "%COB_MAIN_DIR%" == "" (
   echo.
   if "%COB_MAIN_DIR%" == "%~dp0" (
      echo Information: batch was called alread from "%COB_MAIN_DIR%"
      echo              skipping environment setting...
      if not [%1] == [] goto :call_if_needed
      goto :cobcver
   ) else (
      echo Warning: batch was called before from "%COB_MAIN_DIR%"
      echo          resetting COB_CFLAGS, COB_LDFLAGS 
      set "COB_CFLAGS="
      set "COB_LDLAGS="
   )
)

:: Get the main dir from the batch's position (only works in NT environments)
set "COB_MAIN_DIR=%~dp0"

:: settings for cobc
set "COB_CONFIG_DIR=%COB_MAIN_DIR%config"
set "COB_COPY_DIR=%COB_MAIN_DIR%copy"
set "COB_CFLAGS=-I"%COB_MAIN_DIR%include" %COB_CFLAGS%"
set "COB_LDFLAGS=-L"%COB_MAIN_DIR%lib" %COB_LDFLAGS%"

:: settings for libcob
set "COB_LIBRARY_PATH=%COB_MAIN_DIR%extras"

:: Add the bin path of GnuCOBOL (including GCC) to PATH for further references
set "PATH=%COB_MAIN_DIR%bin;%PATH%"

:: Locales
set "LOCALEDIR=%COB_MAIN_DIR%locale"

:: start executable as requested
:call_if_needed
if not [%1] == [] (
  echo environment is prepared:
  call :cobcver
  echo now starting the requested %1
  call %*
  goto :eof
)

:: new cmd to stay open if not started directly from cmd.exe window 
echo %cmdcmdline% | find /i "%~0" >nul
if %errorlevel% equ 0 (
  cmd /k "cobc --version && $versinfo_cmds"
  goto :eof
)

:: Compiler and package version output
:cobcver
echo.
cobc --version
$versinfo_cmds

_FEOF
sed -i 's/$/\r/' "$target_dir/set_env.cmd"


cat > "$target_dir/BUGS.txt" << '_FEOF'

Known bugs found in this distribution, which are normally not in GnuCOBOL n.n:

* NONE

_FEOF
sed -i 's/$/\r/'  "$target_dir/BUGS.txt"


{
	grep -m1 -B100 "==========" "$EXTSRCDIR/README";
	cat  << _FEOF

This package (MinGW based) is intended for testing purposes on Windows systems
and has everything needed to run the compiler and runtime, including the
necessary C compiler.

Version details:
$versinfo

It is NOT optimized and may have some minor bugs other binaries created from the
same source tarball don't have.

Important: See BUGS.txt for possible known issues in this distribution!

For running GnuCOBOL simply double-click set_env.cmd found next to this file, or,
if already in cmd, call setenv.cmd once.
You can use cobc/cobcrun in the command prompt afterwards.

_FEOF
} >> "$target_dir/README.txt"
sed -i 's/$/\r/' "$target_dir/README.txt"


echo && echo duplicating for debug version...
cp -rp "$target_dir" "$target_dir"_dbg

echo && echo stripping binaries...
pushd "$target_dir/bin" 1>/dev/null
strip -p --strip-debug --strip-unneeded ./*.dll ./*.exe 2>/dev/null
cd "../lib"
strip -p --strip-debug --strip-unneeded ./*.a           2>/dev/null
popd 1>/dev/null


echo && echo FINISHED
echo && echo Test the package and adjust BUGS.txt as needed.
