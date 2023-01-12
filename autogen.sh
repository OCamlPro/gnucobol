#!/bin/sh
# autogen.sh gnucobol
# Bootstrap gnucobol package from checked-out sources
# Note:  call as ./autogen.sh if you don't have readlink -f
#
# Copyright (C) 2019,2022,2023 Free Software Foundation, Inc.
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

me=autogen.sh

# get path to GnuCOBOL main directory
if test "$0" = "./$me"; then
  MAINPATH=.
  GCMAINPATH=".."
else
  MAINPATH=$(dirname $(readlink -f "$0"))
  GCMAINPATH="$MAINPATH"
fi
if test ! -f $MAINPATH/$me; then
  echo; echo "ERROR - cannot set main directory [checked $MAINPATH/$me] - aborting $me" && exit 1
fi

olddir_autogen=`pwd`
cd $MAINPATH/build_aux && (chmod -f u+x ./bootstrap; ./bootstrap $*); ret=$?
cd $olddir_autogen

if test $ret -ne 0; then
  exit $ret
fi
