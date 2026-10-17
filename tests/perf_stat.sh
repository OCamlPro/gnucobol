#!/bin/sh
#
# perf_stat.sh gnucobol/tests
#
# Copyright (C) 2026 Free Software Foundation, Inc.
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

command_is_cobc() {
	case "$1" in
        *cobcrun*) return 1 ;;
		*cobc*) return 0 ;;
	esac
	return 1
}

# Notes:

# add additional "-e " if interested in more stats,
# drop the --no-inherit if you want the compute cost for generated code
extra_opt=""

logdir="$1"; shift

# in case of cobc: don't follow child processes (C compiler, linker)
if command_is_cobc "$1"; then
    extra_opt="--no-inherit $extra_opt"
fi

# using a common logfile (with --append); allows to both calculate totals
# to compare different versions, as well as inspecting single entries
outfile="${logdir}/${PERFSUFFIX}.log"
perf stat ${extra_opt} -e instructions --append --output "${outfile}" "$@"
