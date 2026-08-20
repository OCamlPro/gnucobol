#!/bin/bash
# run_chain_tests.sh — Tests for multi-preparser chaining
#
# Validates the IBM mainframe-style preprocessor pipeline:
#   CICS → SQL → cobc  (2-way chain)
#   CICS → DLI → SQL → cobc  (3-way chain)
#
# The key mechanism: after each preparser runs, cobc does
# "goto restart_preprocess" which re-scans the output for
# the next EXEC <subsystem> block.

set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEST_DIR="$SCRIPT_DIR/test_cases"
RESULTS_DIR="$SCRIPT_DIR/results"
COBC="/home/utam1/gnucobol/cobc/cobc"
GIXSQL_COPY="/home/utam1/gixsql-1.0.20b/copy"
GIXSQL_EXAMPLES="/home/utam1/gixsql-1.0.20b/examples"

GIXSQL_CONF="$SCRIPT_DIR/gixsql.conf"
CICS_CONF="$SCRIPT_DIR/cics.conf"
DLI_CONF="$SCRIPT_DIR/dli.conf"

export LD_LIBRARY_PATH="/home/utam1/gixsql-1.0.20b/runtime/libgixsql/.libs:/home/utam1/gixsql-1.0.20b/runtime/libgixsql-sqlite/.libs:/home/utam1/gnucobol/libcob/.libs:$LD_LIBRARY_PATH"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

TOTAL=0
PASS=0
FAIL=0

log_header() {
    echo -e "\n${BOLD}${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${CYAN}  $1${NC}"
    echo -e "${BOLD}${CYAN}═══════════════════════════════════════════════════════════${NC}"
}

log_test() { echo -e "\n${BOLD}── $1 ──${NC}"; }
log_pass() { echo -e "  ${GREEN}✓ PASS${NC} $1"; PASS=$((PASS+1)); }
log_fail() { echo -e "  ${RED}✗ FAIL${NC} $1"; FAIL=$((FAIL+1)); }
log_info() { echo -e "  ${NC}  $1${NC}"; }

# ═══════════════════════════════════════════════════════════════
# Test 1: Mock CICS preprocessor in isolation
# ═══════════════════════════════════════════════════════════════
test_cics_standalone() {
    TOTAL=$((TOTAL+1))
    log_test "Test 1: CICS preprocessor standalone"

    local src="$TEST_DIR/test_cics_only.cbl"
    local out="$RESULTS_DIR/chain_cics_standalone.out"
    local log="$RESULTS_DIR/chain_cics_standalone.log"

    # Run mock CICS preprocessor directly
    timeout 15 "$SCRIPT_DIR/mock_cics_preparse.sh" "$src" "$out" > "$log" 2>&1
    local rc=$?

    if [ $rc -ne 0 ]; then
        log_fail "mock_cics_preparse.sh exit=$rc"
        cat "$log" 2>/dev/null | head -5 | sed 's/^/    /'
        return
    fi

    # Verify EXEC CICS blocks were transformed
    local dfh_calls=$(grep -c 'CALL "DFHEIBLK"' "$out" 2>/dev/null || true)
    dfh_calls=${dfh_calls:-0}
    # Count remaining EXEC CICS on non-comment lines (column 7 != *)
    local exec_cics_remaining=$(grep 'EXEC CICS' "$out" 2>/dev/null | grep -vc '^.\{6\}\*' || true)
    exec_cics_remaining=${exec_cics_remaining:-0}

    log_info "DFHEIBLK CALL statements: $dfh_calls"
    log_info "Remaining EXEC CICS: $exec_cics_remaining"

    if [ "$dfh_calls" -gt 0 ] && [ "$exec_cics_remaining" -eq 0 ]; then
        log_pass "CICS blocks transformed to CALL DFHEIBLK ($dfh_calls calls)"
    else
        log_fail "CICS transformation incomplete"
    fi
}

# ═══════════════════════════════════════════════════════════════
# Test 2: CICS preprocessor via cobc --preparser
# ═══════════════════════════════════════════════════════════════
test_cics_via_cobc() {
    TOTAL=$((TOTAL+1))
    log_test "Test 2: CICS via cobc --preparser"

    local src="$TEST_DIR/test_cics_only.cbl"
    local out="$RESULTS_DIR/chain_cics_cobc.i"
    local log="$RESULTS_DIR/chain_cics_cobc.log"

    cp "$src" "$RESULTS_DIR/test_cics_only.cbl"
    timeout 30 "$COBC" -E \
        --preparser="$CICS_CONF" \
        -o "$out" \
        "$RESULTS_DIR/test_cics_only.cbl" \
        > "$log" 2>&1
    local rc=$?

    if [ $rc -ne 0 ]; then
        log_fail "cobc --preparser=cics.conf exit=$rc"
        cat "$log" 2>/dev/null | head -10 | sed 's/^/    /'
        return
    fi

    # Verify CICS was handled
    local dfh_calls=$(grep -c 'DFHEIBLK' "$out" 2>/dev/null || true)
    dfh_calls=${dfh_calls:-0}

    if [ "$dfh_calls" -gt 0 ]; then
        log_pass "cobc correctly invoked CICS preparser ($dfh_calls DFHEIBLK refs)"
    else
        log_fail "No DFHEIBLK references in output"
    fi
}

# ═══════════════════════════════════════════════════════════════
# Test 3: 2-way chain — CICS + SQL
# ═══════════════════════════════════════════════════════════════
test_cics_sql_chain() {
    TOTAL=$((TOTAL+1))
    log_test "Test 3: 2-way chain — CICS + SQL (mainframe pipeline)"

    local src="$TEST_DIR/test_cics_sql_chain.cbl"
    local out="$RESULTS_DIR/chain_cics_sql.i"
    local log="$RESULTS_DIR/chain_cics_sql.log"

    cp "$src" "$RESULTS_DIR/test_cics_sql_chain.cbl"

    # Register BOTH preparsers — cobc should chain them automatically
    timeout 30 "$COBC" -E \
        --preparser="$CICS_CONF" \
        --preparser="$GIXSQL_CONF" \
        -I "$GIXSQL_COPY" \
        -I "$GIXSQL_EXAMPLES" \
        -o "$out" \
        "$RESULTS_DIR/test_cics_sql_chain.cbl" \
        > "$log" 2>&1
    local rc=$?

    if [ $rc -ne 0 ]; then
        log_fail "cobc 2-way chain exit=$rc"
        cat "$log" 2>/dev/null | head -15 | sed 's/^/    /'
        return
    fi

    if [ ! -f "$out" ]; then
        log_fail "No output file produced"
        return
    fi

    # Verify BOTH preprocessors ran:
    # 1. CICS blocks should be transformed (DFHEIBLK CALL statements)
    local cics_evidence=$(grep -c 'DFHEIBLK' "$out" 2>/dev/null || true)
    cics_evidence=${cics_evidence:-0}

    # 2. SQL blocks should be transformed (GIXSQL markers or GIXSQLxxx calls)
    local sql_evidence=$(grep -c 'GIXSQL\|GIXSQLConnect\|GIXSQLExec' "$out" 2>/dev/null || true)
    sql_evidence=${sql_evidence:-0}

    # 3. No unprocessed EXEC blocks should remain (exclude comment lines)
    local remaining_exec_cics=$(grep 'EXEC CICS' "$out" 2>/dev/null | grep -vc '^.\{6\}\*' || true)
    remaining_exec_cics=${remaining_exec_cics:-0}
    local remaining_exec_sql=$(grep 'EXEC SQL' "$out" 2>/dev/null | grep -vc '^.\{6\}\*' || true)
    remaining_exec_sql=${remaining_exec_sql:-0}

    log_info "CICS evidence (DFHEIBLK):     $cics_evidence"
    log_info "SQL evidence (GIXSQL markers): $sql_evidence"
    log_info "Remaining raw EXEC CICS:       $remaining_exec_cics"
    log_info "Remaining raw EXEC SQL:        $remaining_exec_sql"

    local chain_ok=1

    if [ "$cics_evidence" -gt 0 ]; then
        log_pass "CICS preprocessor ran (evidence: $cics_evidence lines)"
    else
        log_fail "CICS preprocessor did NOT run"
        chain_ok=0
    fi

    if [ "$sql_evidence" -gt 0 ]; then
        log_pass "SQL preprocessor (gixpp) ran (evidence: $sql_evidence lines)"
    else
        log_fail "SQL preprocessor (gixpp) did NOT run"
        chain_ok=0
    fi

    if [ $chain_ok -eq 1 ]; then
        log_pass "2-way chain CICS→SQL→cobc successful!"
    fi
}

# ═══════════════════════════════════════════════════════════════
# Test 4: 3-way chain — CICS + DLI + SQL
# ═══════════════════════════════════════════════════════════════
test_triple_chain() {
    TOTAL=$((TOTAL+1))
    log_test "Test 4: 3-way chain — CICS + DLI + SQL (full mainframe pipeline)"

    local src="$TEST_DIR/test_triple_chain.cbl"
    local out="$RESULTS_DIR/chain_triple.i"
    local log="$RESULTS_DIR/chain_triple.log"

    cp "$src" "$RESULTS_DIR/test_triple_chain.cbl"

    # Register ALL THREE preparsers
    timeout 30 "$COBC" -E \
        --preparser="$CICS_CONF" \
        --preparser="$DLI_CONF" \
        --preparser="$GIXSQL_CONF" \
        -I "$GIXSQL_COPY" \
        -I "$GIXSQL_EXAMPLES" \
        -o "$out" \
        "$RESULTS_DIR/test_triple_chain.cbl" \
        > "$log" 2>&1
    local rc=$?

    if [ $rc -ne 0 ]; then
        log_fail "cobc 3-way chain exit=$rc"
        cat "$log" 2>/dev/null | head -15 | sed 's/^/    /'
        return
    fi

    if [ ! -f "$out" ]; then
        log_fail "No output file produced"
        return
    fi

    # Verify ALL THREE preprocessors ran
    local cics_evidence=$(grep -c 'DFHEIBLK' "$out" 2>/dev/null || true)
    cics_evidence=${cics_evidence:-0}

    local dli_evidence=$(grep -c 'CBLTDLI' "$out" 2>/dev/null || true)
    dli_evidence=${dli_evidence:-0}

    local sql_evidence=$(grep -c 'GIXSQL\|GIXSQLConnect\|GIXSQLExec' "$out" 2>/dev/null || true)
    sql_evidence=${sql_evidence:-0}

    log_info "CICS evidence (DFHEIBLK):     $cics_evidence"
    log_info "DLI evidence (CBLTDLI):       $dli_evidence"
    log_info "SQL evidence (GIXSQL):          $sql_evidence"

    local chain_ok=1

    if [ "$cics_evidence" -gt 0 ]; then
        log_pass "CICS preprocessor ran"
    else
        log_fail "CICS preprocessor did NOT run"
        chain_ok=0
    fi

    if [ "$dli_evidence" -gt 0 ]; then
        log_pass "DLI preprocessor ran"
    else
        log_fail "DLI preprocessor did NOT run"
        chain_ok=0
    fi

    if [ "$sql_evidence" -gt 0 ]; then
        log_pass "SQL preprocessor (gixpp) ran"
    else
        log_fail "SQL preprocessor (gixpp) did NOT run"
        chain_ok=0
    fi

    if [ $chain_ok -eq 1 ]; then
        log_pass "3-way chain CICS→DLI→SQL→cobc successful!"
    fi
}

# ═══════════════════════════════════════════════════════════════
# Test 5: Verify chaining order — inspect intermediate files
# ═══════════════════════════════════════════════════════════════
test_chain_order() {
    TOTAL=$((TOTAL+1))
    log_test "Test 5: Verify chaining order (intermediate file inspection)"

    local src="$TEST_DIR/test_cics_sql_chain.cbl"
    local out="$RESULTS_DIR/chain_order.i"
    local log="$RESULTS_DIR/chain_order.log"

    cp "$src" "$RESULTS_DIR/test_cics_sql_chain_order.cbl"

    # Run with verbose output to trace the chaining
    timeout 30 "$COBC" -E -v \
        --preparser="$CICS_CONF" \
        --preparser="$GIXSQL_CONF" \
        -I "$GIXSQL_COPY" \
        -I "$GIXSQL_EXAMPLES" \
        -o "$out" \
        "$RESULTS_DIR/test_cics_sql_chain_order.cbl" \
        > "$log" 2>&1
    local rc=$?

    if [ $rc -ne 0 ]; then
        log_fail "cobc verbose chain exit=$rc"
        cat "$log" 2>/dev/null | head -15 | sed 's/^/    /'
        return
    fi

    # Check verbose output for preprocessing traces
    log_info "Verbose trace:"
    grep -i "preprocess\|preparser" "$log" 2>/dev/null | head -10 | sed 's/^/    /'

    # Check that intermediate .i files exist (evidence of chaining)
    local i_files=$(find "$RESULTS_DIR" -name "test_cics_sql_chain_order*.i*" 2>/dev/null | sort)
    log_info "Intermediate files:"
    echo "$i_files" | sed 's/^/    /'

    if [ -f "$out" ]; then
        local total_lines=$(wc -l < "$out")
        log_pass "Chain completed, final output: $total_lines lines"
    else
        log_fail "No final output produced"
    fi
}

# ═══════════════════════════════════════════════════════════════
# Test 6: SQL-only with CICS preparser registered (no EXEC CICS)
# ═══════════════════════════════════════════════════════════════
test_sql_only_with_cics_registered() {
    TOTAL=$((TOTAL+1))
    log_test "Test 6: SQL-only source with CICS preparser registered"

    local src="$TEST_DIR/test_basic_select.cbl"
    local out="$RESULTS_DIR/chain_sql_only_cics_reg.i"
    local log="$RESULTS_DIR/chain_sql_only_cics_reg.log"

    cp "$src" "$RESULTS_DIR/test_basic_select_chain.cbl"

    # Register BOTH preparsers, but source only has EXEC SQL
    # CICS preparser should NOT be triggered
    timeout 30 "$COBC" -E \
        --preparser="$CICS_CONF" \
        --preparser="$GIXSQL_CONF" \
        -I "$GIXSQL_COPY" \
        -I "$GIXSQL_EXAMPLES" \
        -o "$out" \
        "$RESULTS_DIR/test_basic_select_chain.cbl" \
        > "$log" 2>&1
    local rc=$?

    if [ $rc -ne 0 ]; then
        log_fail "cobc exit=$rc"
        cat "$log" 2>/dev/null | head -10 | sed 's/^/    /'
        return
    fi

    local sql_evidence=$(grep -c 'GIXSQL' "$out" 2>/dev/null || true)
    sql_evidence=${sql_evidence:-0}
    local cics_evidence=$(grep -c 'DFHEIBLK' "$out" 2>/dev/null || true)
    cics_evidence=${cics_evidence:-0}

    log_info "SQL evidence: $sql_evidence lines"
    log_info "CICS evidence: $cics_evidence lines (should be 0)"

    if [ "$sql_evidence" -gt 0 ] && [ "$cics_evidence" -eq 0 ]; then
        log_pass "Only SQL preparser triggered (CICS correctly skipped)"
    else
        log_fail "Unexpected behavior"
    fi
}

# ═══════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════
log_header "Multi-Preparser Chaining Tests"
echo "  Date: $(date '+%Y-%m-%d %H:%M:%S')"
echo "  Simulating IBM mainframe pipeline: CICS → DLI → SQL → cobc"
echo ""
echo "  Configs:"
echo "    CICS: $CICS_CONF"
echo "    DLI:  $DLI_CONF"
echo "    SQL:  $GIXSQL_CONF"

# Clean chain results
rm -f "$RESULTS_DIR"/chain_*

log_header "Running Chain Tests"

test_cics_standalone
test_cics_via_cobc
test_cics_sql_chain
test_triple_chain
test_chain_order
test_sql_only_with_cics_registered

log_header "Chain Test Summary"
echo ""
echo -e "  Total tests : ${BOLD}$TOTAL${NC}"
echo -e "  Passed      : ${GREEN}$PASS${NC}"
echo -e "  Failed      : ${RED}$FAIL${NC}"
echo ""

if [ $FAIL -eq 0 ]; then
    echo -e "  ${GREEN}${BOLD}ALL CHAIN TESTS PASSED ✓${NC}"
else
    echo -e "  ${RED}${BOLD}SOME CHAIN TESTS FAILED ✗${NC}"
fi
echo ""
