#!/bin/bash
# mock_cics_preparse.sh — Mock CICS preprocessor for testing preparser chaining
#
# This simulates a CICS preprocessor in the mainframe pipeline:
#   CICS → SQL → cobc
#
# It transforms EXEC CICS ... END-EXEC blocks into COBOL CALL statements
# (calling the CICS DFHEIBLK interface), while leaving EXEC SQL blocks
# and all other COBOL code untouched.
#
# Called by cobc as: mock_cics_preparse.sh <source> <output> [include_paths]

set -e

SOURCE="$1"
OUTPUT="$2"

if [ -z "$SOURCE" ] || [ -z "$OUTPUT" ]; then
    echo "Usage: $0 <source_file> <output_file> [include_paths]" >&2
    exit 1
fi

if [ ! -f "$SOURCE" ]; then
    echo "ERROR: Source file not found: $SOURCE" >&2
    exit 1
fi

# Transform EXEC CICS ... END-EXEC blocks:
#   1. Comment out original EXEC CICS lines by placing '*' at column 7
#      (valid COBOL fixed-format comment indicator)
#   2. Insert CALL "DFHEIBLK" replacement code in proper column alignment
#
# COBOL fixed format:
#   Columns 1-6:  sequence number area
#   Column  7:    indicator area (* = comment, space = code)
#   Columns 8-11: Area A (division/section/paragraph headers)
#   Columns 12-72: Area B (statements)

awk '
BEGIN {
    in_cics = 0
    cics_verb = ""
}

# Detect start of EXEC CICS block
/EXEC[[:space:]]+CICS/ {
    in_cics = 1
    # Extract the CICS verb (SEND, RECEIVE, RETURN, etc.)
    match($0, /EXEC[[:space:]]+CICS[[:space:]]+([A-Z]+)/, arr)
    if (arr[1] != "") {
        cics_verb = arr[1]
    } else {
        cics_verb = "UNKNOWN"
    }
}

# Comment out a line by placing * at column 7 (COBOL fixed-format comment)
function comment_out(line) {
    # Ensure line is at least 7 chars; pad with spaces if needed
    while (length(line) < 7) {
        line = line " "
    }
    # Replace column 7 (index 7, 1-based) with *
    return substr(line, 1, 6) "*" substr(line, 8)
}

# Inside EXEC CICS block — comment out lines
in_cics == 1 {
    print comment_out($0)

    # Check for END-EXEC
    if ($0 ~ /END-EXEC/) {
        in_cics = 0
        # Emit replacement CALL statement in proper COBOL columns
        # Column 12 = Area B for statements
        printf "           CALL \"DFHEIBLK\" USING\n"
        printf "               BY CONTENT \"%s\"\n", cics_verb
        printf "               BY REFERENCE DFHEIBLK\n"
        printf "               BY REFERENCE DFHCOMMAREA\n"
        printf "           END-CALL.\n"
    }
    next
}

# Pass through all other lines unchanged (including EXEC SQL blocks)
{ print }
' "$SOURCE" > "$OUTPUT"

exit 0

