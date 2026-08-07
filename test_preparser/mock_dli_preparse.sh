#!/bin/bash
# mock_dli_preparse.sh — Mock IMS/DLI preprocessor for testing preparser chaining
#
# This simulates an IMS/DL/I preprocessor. In the IBM pipeline:
#   SQL → DLI → CICS → cobc
#
# It transforms EXEC DLI ... END-EXEC blocks into COBOL CALL statements
# (calling the IMS CBLTDLI interface), while leaving EXEC SQL, EXEC CICS,
# and all other COBOL code untouched.
#
# Called by cobc as: mock_dli_preparse.sh <source> <output> [include_paths]

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

# Transform EXEC DLI ... END-EXEC blocks:
#   1. Comment out original EXEC DLI lines by placing '*' at column 7
#   2. Insert CALL "CBLTDLI" replacement code in proper columns
#
# COBOL fixed format:
#   Columns 1-6:  sequence number area
#   Column  7:    indicator area (* = comment, space = code)
#   Columns 8-11: Area A
#   Columns 12-72: Area B (statements)

awk '
BEGIN {
    in_dli = 0
    dli_verb = ""
}

# Detect start of EXEC DLI block
/EXEC[[:space:]]+DLI/ {
    in_dli = 1
    # Extract the DLI verb (GU, GN, GNP, ISRT, REPL, DLET, etc.)
    match($0, /EXEC[[:space:]]+DLI[[:space:]]+([A-Z]+)/, arr)
    if (arr[1] != "") {
        dli_verb = arr[1]
    } else {
        dli_verb = "GU"
    }
}

# Comment out a line by placing * at column 7 (COBOL fixed-format comment)
function comment_out(line) {
    while (length(line) < 7) {
        line = line " "
    }
    return substr(line, 1, 6) "*" substr(line, 8)
}

# Inside EXEC DLI block — comment out lines
in_dli == 1 {
    print comment_out($0)
    if ($0 ~ /END-EXEC/) {
        in_dli = 0
        # Emit replacement CALL CBLTDLI statement in proper columns
        printf "           CALL \"CBLTDLI\" USING\n"
        printf "               DLI-FUNC\n"
        printf "               PCB-POINTER\n"
        printf "               IO-AREA\n"
        printf "               SSA1\n"
        printf "           END-CALL.\n"
    }
    next
}

# Pass through all other lines unchanged
{ print }
' "$SOURCE" > "$OUTPUT"

exit 0

