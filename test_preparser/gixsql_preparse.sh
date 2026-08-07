#!/bin/bash
# gixsql_preparse.sh — Bridge script for GnuCOBOL preparser → GixSQL gixpp
#
# cobc invokes external preparsers as:
#     <command> <source_file> <output_file> ["<include_paths>"]
#
# The third argument (if present) is a quoted string of -I flags
# forwarded from cobc, e.g. "-I /path/one -I /path/two".
#
# gixpp expects:
#     gixpp -e -i <input> -o <output> [-I <copypath>] [options]
#
# This script translates between the two interfaces.

set -e

GIXPP="/home/utam1/gixsql-1.0.20b/gixpp/gixpp"

# Copy path directories for SQLCA.cpy, EMPREC.cpy, etc.
GIXSQL_COPY="/home/utam1/gixsql-1.0.20b/copy"
GIXSQL_EXAMPLES="/home/utam1/gixsql-1.0.20b/examples"

SOURCE="$1"
OUTPUT="$2"
INCLUDE_PATHS="$3"

if [ -z "$SOURCE" ] || [ -z "$OUTPUT" ]; then
    echo "Usage: $0 <source_file> <output_file> [include_paths]" >&2
    exit 1
fi

# Build include args: start with gixsql-specific copy paths,
# then append any -I paths forwarded from cobc
INCLUDE_ARGS="-I $GIXSQL_COPY -I $GIXSQL_EXAMPLES"

if [ -n "$INCLUDE_PATHS" ]; then
    INCLUDE_ARGS="$INCLUDE_ARGS $INCLUDE_PATHS"
fi

# Run gixpp ESQL preprocessor
exec "$GIXPP" -e \
    -i "$SOURCE" \
    -o "$OUTPUT" \
    $INCLUDE_ARGS
