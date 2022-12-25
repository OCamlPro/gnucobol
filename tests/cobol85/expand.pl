#
# gnucobol/tests/cobol85/expand.pl
#
# Copyright (C) 2001-2012, 2019-2020, 2022 Free Software Foundation, Inc.
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

use strict;

# use warnings, if available
# warnings is only a core module since Perl 5.6
BEGIN { eval "use warnings;" }

my $input = shift;
my $module = shift;
if ($input  eq "") {die "missing argument: input file";}
if ($module eq "") {die "missing argument: module output directory";}
open (IN, $input) or die "input file \"$input\" not found";

my $output = '';
while (<IN>) {
	s/\x0d\x0a|\x0d|\x0a//g;
	if (/^      \*HEADER,([^,]*),([^, ]*)(,([^,]*),([^, ]*))?/) {
		my ($type, $prog, $subt, $subr) = ($1, $2, $4, $5);
		$output = $type;
		my $name = '';
		if ($subt) {
			if ($subt eq "SUBPRG") {
				$name = "$subr.SUB";
			} elsif ($subt eq "SUBRTN") {
				$name = "lib/$subr.CBL";
				mkdir "$module/lib",0755 unless (-e "$module/lib");
			}
		} elsif ($type eq "COBOL") {
			$name = "$prog.CBL";
		} elsif ($type eq "DATA*") {
			if (substr($prog, 0, 2) eq $module) {
				$name = "$prog.DAT";
			}
		} elsif ($type eq "CLBRY") {
			if ($prog eq "ALTL1") {
				$module = "copyalt";
				$name = "ALTLB" unless (-e "copyalt/ALTLB");
			} else {
				$module = "copy";
				$name = "$prog" unless (-e "copy/$prog");
			}
		}
		if ($name) {
			mkdir $module,0755 unless (-e $module);
			open (OUT, "> $module/$name") or die;
			while (<IN>) {
				last if /^      \*END/;
				if ($type eq "DATA*" and length >= 80) {
					s/\x0d\x0a|\x0d|\x0a//g;
				}
				print OUT;
			}
		} else {
			while (<IN>) {
				last if /^      \*END/;
			}
		}
	}
}
if ($output eq "") {die "input file \"$input\" does not contain data to expand";}
