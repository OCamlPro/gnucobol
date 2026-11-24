#!/bin/bash

# Copyright (C) 2022-2023, 2026 Free Software Foundation, Inc.
# Written by Jonathan Hilger, Simon Sobisch
#
# This file is part of GnuCOBOL.
#
# The GnuCOBOL toolset is free software: you can redistribute it
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

desc='
 Script to aggregate or average perf-stat from files that were created with
     perf stat [-e ...] --output=file --append
'
version=261123

# CHANGES:
#    jh: 220720  initial version
#    so: 220721  multiple files, input validation, reformatting
#    jh: 220725  dynamic events
#    so: 220922  argument handling, minor cleanup
#    so: 230413  fixes for argument handling, cleanup for dynamic events
#    so: 231117  handling of unsupported events and msec,
#                simplified dynamic event handling, added --average
#    so: 261123  skip binary files, prefer awk over external bc binary

version_fkt() {
  echo "${0##*/} - version: $version (date)"
  echo "$desc"
  exit 0
}

usage_fkt() {
  echo "Usage: ${0##*/} [OPTION] [FILE...]"
}

broken_usage_fkt() {
  usage_fkt
  echo "${0##*/} --help for more information"
  exit 2
}

help_fkt() {
  usage_fkt
  echo "$desc"
  echo "Options:"
  echo " --help      show this help"
  echo " --version   show version"
  echo " --events    EVENT[,EVENT,...] comma-separated list of events"
  echo "             that are aggregated and shown, default: all that are included"
  echo " --average   get average, not aggregation"
  exit 0
}

[ "$1" == "" ] && broken_usage_fkt
z_events_glob="none"
z_files=""
z_do_average=""

for z_opt in "$@"; do
  case "$z_opt" in
  "--help"    | "-?" ) help_fkt    ;;
  "--version" | "-V" ) version_fkt ;;
  "--average" | "-a" ) z_do_average=x ;;
  "--events"  | "-e" ) z_events_glob=next ;;
  -*)                  broken_usage_fkt   ;;
  *)
    if [ "$z_events_glob" = "next" ] ; then
      z_events_glob="$z_opt"
    elif [ "$z_files" = "" ] ;  then
      z_files="$z_opt"
    else
      z_files="$z_files,$z_opt"
    fi
    ;;
  esac
done
[ "$z_events_glob" = "next" ] && broken_usage_fkt

IFS=','

for z_file in $z_files; do

  echo ""
  if [ ! -f "$z_file" ] ; then
    echo "File '$z_file' does not exist!" ; echo ""; echo ""
    continue
  fi
  # check for binary file - is just skipped if grep ios not GNU compatible
  if grep -aPq '\x00' -- "$z_file" 2>/dev/null; then
    echo "File '$z_file' seems to be binary!" ; echo ""; echo ""
    continue
  fi
  if [ "$z_do_average" = "" ] ; then
    echo "Aggregation for $z_file:"; echo ""
  else
    echo "Average for $z_file:"; echo ""
  fi

  # events we're interested in:
  z_events=""

  # get events from entries in the file
  eventlist="$(sed 's/ *#.*// ; /^ *$/d ; s/^ *//g ; / *Performance counter.*/d ; s/\([[:digit:]]\|\.\|,\)\+ *//g ; s/ *$//g ; s/$/,/g' "$z_file")"

  for z_item in $eventlist; do
      # drop leading newlines
      z_item=${z_item#*$'\n'}

      # ignore duplicates
      if [[ $z_events =~ $z_item ]]; then
         continue
      fi
      # insert into unique list
      if [ "$z_events_glob" != none ] ; then
        # events per command line - only include in list if also in command line
        found=
        for cmd_event in $z_events_glob; do
            if [[ $z_item =~ $cmd_event ]]; then
               found=x
               break
            fi
        done
        if [ "$found" = "" ] ; then
          continue
        fi
      fi

      # if empty: set and get out
      if [[ $z_events = "" ]]; then
         z_events=$z_item
         continue
      fi
     z_events=$z_events,$z_item
  done

  for z_entry in $z_events ; do
    # aggregation / average

    if [ "$z_do_average" = "" ] ; then
      z_num=1
    else
      z_num=$(grep "$z_entry" -c "$z_file")
    fi
    
    if ! [[ "$z_entry" =~ "not supported" ]]; then
       z_total=$(awk -v entry="$z_entry" '
         index($0, entry) {
           gsub(/[,.]/, "", $1)
           total += $1
         }
         END { print total+0 }
       ' "$z_file")
    fi
    if [[ "$z_entry" =~ ^seconds ]]; then
      # formatted for time:
      # perf prints to full nanocseconds, we dropped the numeric formatting and have to calculate it in
      # as the details are not relevant (we do have a scale of 8) we only give out a scale of 4
      z_value=$(awk -v total="$z_total" -v num="$z_num" 'BEGIN {
        printf "%.4f\n", total / num / 1000000000
      }')
    elif [[ "$z_entry" =~ ^msec ]]; then
      # formatted for time:
      # perf prints some entries like task-clock as secs with scale of two, we dropped the numeric formatting and have to calculate
      z_value=$(awk -v total="$z_total" -v num="$z_num" 'BEGIN {
        printf "%.2f\n", total / num / 100
      }')
    elif [[ "$z_entry" =~ "not supported" ]]; then
      # no calculation for unsupported events
      z_value="${z_entry%%  *}"
      z_entry="${z_entry#*  }"
    else
      # formatted for numbers:
      z_value=$(awk -v total="$z_total" -v num="$z_num" 'BEGIN {
        printf "%d\n", total / num
      }')
      z_value=$(numfmt --grouping "$z_value")
    fi
    printf "%20s%18s\n" "$z_entry" "$z_value"
  done

  echo ""
done
