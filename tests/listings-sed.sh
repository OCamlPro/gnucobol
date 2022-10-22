#! /bin/sh
#
# listings-sed.sh gnucobol/tests
#
# Copyright (C) 2016-2017, 2020-2022 Free Software Foundation, Inc.
# Written by Simon Sobisch, David Pitts
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

# Necessary sed replacements for unifying a listing

# Note: We cater for a maximum version string of 14:
#       Mayor (2) '.' Minor (2) '.' Patchlevel (8 - as some people place a date here)
# Note: We replace the date two times, as not all systems have %e modifier in C
#       and use %E in this case ("Mon Feb 04" instead of "Mon Feb  4").
# Note: after all parts are replaced (the date may need a local adjustment depending
#       on configure specified LISTING_TIMESTAMP_FORMAT) we replace those
#       with the ANSI format which is expected in the listing test references

date1=`date +"%a %b %e"`
date2=`date +"%a %b %d"`
year=`date +"%Y"`

if test "x$SED" = "x" ; then SED=sed ; fi

if test "$3" = "once"; then
	$SED \
	-e 's/GnuCOBOL [0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*[-devalphabetarcpviw]*[0-9]*\.[0-9][0-9]*  */GnuCOBOL V.R.P               /g' \
	-e 's/GnuCOBOL [0-9][0-9]*\.[0-9][0-9]*[-devalphabetarcpviw]*[0-9]*\.[0-9][0-9]*  */GnuCOBOL V.R.P               /g' \
	-e 's/'"$date1"'/DDD MMM dd/g' \
	-e 's/'"$date2"'/DDD MMM dd/g' \
	-e 's/'"$year"'/YYYY/g' \
	-e 's/[0-2][0-9]:[0-6][0-9]:[0-9][0-9]/HH:MM:SS/g' \
	-e 's/DDD MMM dd YYYY HH:MM:SS/DDD MMM dd HH:MM:SS YYYY/g' \
	<"$1" >"$2"
else
	$SED \
	-e 's/GnuCOBOL [0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*[-devalphabetarcpviw]*[0-9]*\.[0-9][0-9]*  */GnuCOBOL V.R.P          /g' \
	-e 's/GnuCOBOL [0-9][0-9]*\.[0-9][0-9]*[-devalphabetarcpviw]*[0-9]*\.[0-9][0-9]*  */GnuCOBOL V.R.P          /g' \
	-e 's/'"$date1"'/DDD MMM dd/g' \
	-e 's/'"$date2"'/DDD MMM dd/g' \
	-e 's/'"$year"'/YYYY/g' \
	-e 's/[0-2][0-9]:[0-6][0-9]:[0-9][0-9]/HH:MM:SS/g' \
	-e 's/DDD MMM dd YYYY HH:MM:SS/DDD MMM dd HH:MM:SS YYYY/g' \
	<"$1" >"$2"
fi

