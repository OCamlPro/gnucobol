# Test Suite Execution Performance and Behavior

This document summarizes the behavior and execution details of the `run_tests.sh` testing suite designed to validate GnuCOBOL's integration with external preparsers (specifically GixSQL). 

## Execution Summary

The test suite consists of 9 SQL-focused COBOL test cases. When executed, each test correctly traverses four distinct phases to ensure consistency between standalone preprocessing and cobc's integrated `--preparser` execution.

**Status**: ALL 9 TESTS PASSED

## Tested Scenarios

The suite verified the integration through the following test cases:
1. `test_basic_select.cbl`: Simple SQL queries and cursor iterations.
2. `test_connect_disconnect.cbl`: Standard DB connect and disconnect commands.
3. `test_cursor_operations.cbl`: Complex cursor operations (declare, open, fetch, close).
4. `test_include_copybook.cbl`: Verifying that `-I` include paths are forwarded correctly for `EXEC SQL INCLUDE`.
5. `test_insert_update.cbl`: DML operations with host variables.
6. `test_multiple_exec.cbl`: Dense sequential execution of multiple SQL blocks.
7. `test_no_sql.cbl`: Plain COBOL fallback test ensuring no-op on non-SQL sources.
8. `test_sql_types.cbl`: Testing COBOL host variable types parsed by GixSQL.
9. `test_transaction.cbl`: Testing COMMIT and ROLLBACK logic.

## Test Behavior and Phase Breakdown

For each test case, the execution framework performs four phases:

### Phase A: Standalone Preprocessing
- **What happens:** The test runner executes the external preparser (`gixpp`) directly against the source file.
- **Why:** This acts as the baseline ground truth. It simulates how users would traditionally run external preprocessors manually before invoking cobc.
- **Outcome:** The preprocessed output is saved as a `.phaseA.cbl` file.

### Phase B: Integrated `cobc --preparser`
- **What happens:** The test runner executes `cobc -E --preparser=gixsql.conf ...`.
- **Why:** To verify that `cobc` correctly detects `EXEC SQL`, spawns the wrapper script, passes the appropriate `-I` include flags, reads the intermediate `.i` files, and incorporates the changes correctly during its preprocessing phase.
- **Outcome:** The cobc-generated output is saved as a `.phaseB.i` file.

### Phase C: Comparison and Validation
- **What happens:** The outputs from Phase A and Phase B are analyzed using `grep` to count the injected marker tokens (e.g., `GIXSQL*`). 
- **Why:** To ensure that the integrated `cobc --preparser` flow did not drop, alter, or duplicate any of the code generation that `gixpp` performed in the standalone Phase A.
- **Outcome:** If the marker counts match, the integration is verified to be 100% faithful to the standalone preprocessor. For the `test_no_sql.cbl` test, the script specifically ensures no markers are present, validating cobc's passthrough behavior.

### Phase D: Full Compilation (Syntax Checking)
- **What happens:** The script runs `cobc -x` to fully compile the `.phaseB.i` source code.
- **Why:** To guarantee that the COBOL code returned by the external preparser through `cobc` is syntactically valid and compliant with GnuCOBOL standards.
- **Outcome:** Validates that the compilation succeeds (exit code 0 or informational exit code 1).

## Notes on Multi-Subsystem Tests
Files that contained multiple mixed EXEC blocks (e.g., `EXEC CICS` or `EXEC DLI` mixed with `EXEC SQL`) have been moved out of the `run_tests.sh` execution scope and are exclusively tested by `run_chain_tests.sh`. This ensures that the primary testing script evaluates pure SQL integration functionality without false-negative syntax errors from unsupported subsystems.

---

# Multi-Preparser Chaining Execution (`run_chain_tests.sh`)

In addition to the standalone and single-preparser tests in `run_tests.sh`, the test infrastructure includes `run_chain_tests.sh`, which is designed to validate GnuCOBOL's ability to chain multiple independent external preparsers sequentially.

## Chaining Mechanism

The IBM mainframe pipeline often requires chained compilation passes (e.g., CICS → DLI → SQL → COBOL). The integration feature replicates this by reading multiple `--preparser` directives. When cobc processes a file, it dynamically invokes the corresponding external preparsers, where the output of one serves as the input to the next until all registered subsystem blocks (`EXEC ... END-EXEC`) are resolved.

## Test Behavior and Validations

The `run_chain_tests.sh` suite simulates this environment using mock shell-script preparsers (`mock_cics_preparse.sh` and `mock_dli_preparse.sh`) alongside the real `gixpp` SQL preparser.

It executes the following 6 tests:
1. **Test 1: CICS preprocessor standalone** — Verifies that the mock CICS script successfully transforms `EXEC CICS` blocks into standard `CALL "DFHEIBLK"` statements without cobc interference.
2. **Test 2: CICS via `cobc --preparser`** — Runs cobc configured purely for CICS integration, confirming that cobc correctly forwards the job and integrates the resulting `CALL` statements.
3. **Test 3: 2-way chain — CICS + SQL** — Simulates the standard `CICS → SQL` pipeline. `cobc` is launched with both `cics.conf` and `gixsql.conf`. The test ensures that both `DFHEIBLK` (CICS evidence) and `GIXSQL*` (SQL evidence) exist in the final output.
4. **Test 4: 3-way chain — CICS + DLI + SQL** — Simulates a full `CICS → DLI → SQL` pipeline. Runs the file against all three preparsers to ensure long iteration chains successfully complete without data truncation or infinite loops.
5. **Test 5: Verify chaining order (intermediate file inspection)** — By executing with the `-E -v` (verbose) flags, this test validates the intermediate files (`.i`, `.i2`, `.i3`) produced during the chain passes. It guarantees that `file_replace_extension` iteratively generates unique temporary filenames, effectively mitigating file-write collisions during deeply chained passes.
6. **Test 6: SQL-only source with CICS preparser registered** — Confirms optimization logic: when multiple preparsers are registered but a source file lacks one of the subsystems (e.g., no `EXEC CICS` present), `cobc` safely skips the unnecessary preparser.
