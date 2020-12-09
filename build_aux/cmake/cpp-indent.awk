#! /usr/bin/awk -f
#
# Filter preprocessor statements.
# Indent and comment for clarity.
#

BEGIN {
    fmtif="%4d: %*s%s\n"
    fmt="%4d: %*s%s // from %s\n"
    tabs=""
    depth = 0
}

/# *if/ {
    printf fmtif, NR, depth, tabs, $0
    depth += 2
    frame[depth] = NR
}

/# *el/ {
    closing = frame[depth]
    frame[depth] = NR
    depth -= 2
    printf fmt, NR, depth, tabs, $0, closing
    depth += 2
}    

/# *endif/ {
    closing = frame[depth]
    depth -= 2
    printf fmt, NR, depth, tabs, $0, closing
}       

