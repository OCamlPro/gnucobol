#!/bin/sh
#
# perf_record.sh gnucobol/tests
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

# Warning, using "perf record" may only work with a single job!
# _especially_ when /proc/sys/kernel/perf_event_mlock_kb is small (< 8192),
# also some tests may fail with additional restrictions, if you want to
# record those "globally" then running as root may be necessary

# if you get a warning "Couldn't record kernel reference relocation symbol",
# then run the tests as root or adjust /proc/sys/kernel/kptr_restrict

# if your system has a lower frequency configured than perf's default:
# run as root or adjust /proc/sys/kernel/perf_event_max_sample_rate or add -F max

# run the tests with a single or only few jobs, otherwise tests may fail because of
# an aborted perf (return code 255) ... or use a small memory size "-m 8" to the args below

# if your system does not support that, then drop --aio / -z
extra_opt="--aio -z"

logdir="$1"; shift

# in case of cobc: don't follow child processes (C compiler, linker)
if command_is_cobc "$1"; then
    extra_opt="--no-inherit $extra_opt"
fi

# using a unique filename each time the function is called
outfile="${logdir}/perf.data.${PERFSUFFIX}-${at_group}-$(date +%s%N)"

# note: we use a "relative" high call stack-size, to cover all uses in the testsuite
perf record -q ${extra_opt} --call-graph dwarf,4096 --output "${outfile}" "$@"
