# GnuCOBOL External Preparser — Integration Guide

This directory contains test infrastructure for the GnuCOBOL external preparser
feature, which allows COBOL programs containing `EXEC SQL`, `EXEC CICS`,
`EXEC DLI`, or other subsystem-specific blocks to be preprocessed by external
tools before the GnuCOBOL compiler processes them.

---

## How It Works

When `cobc` encounters an `EXEC <subsystem>` block (e.g., `EXEC SQL`), it checks
if an external preparser is registered for that subsystem. If one is found,
`cobc` invokes the external tool to transform the source file, then re-processes
the transformed output.

The external preparser is invoked as:

```
<command> <input_file> <output_file> ["<include_paths>"]
```

- `$1` — The input COBOL source file
- `$2` — The output file (the preprocessed result)
- `$3` — *(Optional)* A quoted string of `-I` include paths from `cobc`
  (e.g., `"-I /path/one -I /path/two"`)

---

## Step-by-Step: Integrating GixSQL (EXEC SQL) with GnuCOBOL

### Prerequisites

- GnuCOBOL built with preparser support
- [GixSQL](https://github.com/mridoni/gixsql) installed (the `gixpp` preprocessor)

### 1. Create a Configuration File

Create a file named `gixsql.conf` (you can place it anywhere):

```
# gixsql.conf — GixSQL preparser configuration for GnuCOBOL
subsystem:  SQL
command:    /path/to/gixpp-wrapper.sh
cflags:     -I/path/to/gixsql/include
ldflags:    -L/path/to/gixsql/lib -lgixsql -lgixsql-static
on-error:   error
```

**Configuration keys:**

| Key         | Description                                                |
|-------------|------------------------------------------------------------|
| `subsystem` | The EXEC tag this preparser handles (e.g., `SQL`, `CICS`). Required. |
| `command`   | The external command/script to invoke. Required.           |
| `cflags`    | Extra C compiler flags for the preprocessed output.        |
| `ldflags`   | Extra linker flags.                                        |
| `on-error`  | `error` (default, abort) or `warn` (continue on failure).  |

### 2. Create a Wrapper Script

GixSQL's `gixpp` tool uses its own argument format (`-e -i <input> -o <output>`),
which differs from what `cobc` passes. Create a wrapper script to translate:

```bash
#!/bin/bash
# gixpp-wrapper.sh — Bridge between cobc and gixpp
set -e

GIXPP="/path/to/gixpp"
GIXSQL_COPY="/path/to/gixsql/copy"

SOURCE="$1"
OUTPUT="$2"
INCLUDE_PATHS="$3"

# Build include args from cobc-forwarded -I paths
INCLUDE_ARGS="-I $GIXSQL_COPY"
if [ -n "$INCLUDE_PATHS" ]; then
    INCLUDE_ARGS="$INCLUDE_ARGS $INCLUDE_PATHS"
fi

exec "$GIXPP" -e -i "$SOURCE" -o "$OUTPUT" $INCLUDE_ARGS
```

Make it executable: `chmod +x gixpp-wrapper.sh`

### 3. Compile Your COBOL Program

```bash
cobc -x --preparser=./gixsql.conf my_program.cob
```

Or, if you placed `gixsql.conf` in the GnuCOBOL config directory
(`$COB_CONFIG_DIR`), you can use just the name:

```bash
cobc -x --preparser=gixsql my_program.cob
```

### 4. Pass Include Paths

If your COBOL program uses `EXEC SQL INCLUDE` statements that reference
copybooks in specific directories, pass them with `-I`:

```bash
cobc -x --preparser=./gixsql.conf -I /my/copy/dir my_program.cob
```

The `-I` paths are automatically forwarded to the preparser as the third
argument.

---

## Multi-Preparser Chaining

You can register multiple preparsers for different EXEC subsystems. GnuCOBOL
chains them automatically:

```bash
cobc -x \
    --preparser=./cics.conf \
    --preparser=./gixsql.conf \
    my_cics_sql_program.cob
```

**How chaining works:**

1. `cobc` preprocesses the source and detects `EXEC SQL` → invokes the SQL
   preparser (gixpp)
2. The preprocessed output is re-scanned → `EXEC CICS` is detected → invokes
   the CICS preparser
3. The chain repeats until no more EXEC blocks trigger preparsers
4. Final output goes through GnuCOBOL's internal preprocessing

Each preparser in the chain receives the output of the previous one as input.

---

## Test Infrastructure in This Directory

| File / Directory            | Purpose                                                  |
|-----------------------------|----------------------------------------------------------|
| `gixsql.conf`              | GixSQL preparser configuration (used by tests)           |
| `gixsql_preparse.sh`       | Wrapper script bridging cobc → gixpp                     |
| `cics.conf`                | Mock CICS preparser configuration                        |
| `mock_cics_preparse.sh`    | Mock CICS preprocessor (transforms EXEC CICS → CALL)     |
| `dli.conf`                 | Mock DLI preparser configuration                         |
| `mock_dli_preparse.sh`     | Mock DLI preprocessor (transforms EXEC DLI → CALL)       |
| `test_cases/`              | COBOL source files for various test scenarios             |
| `run_tests.sh`             | Runs GixSQL integration tests (single preparser)         |
| `run_chain_tests.sh`       | Runs multi-preparser chaining tests                      |
| `results/`                 | Test output directory (created at runtime)                |

### Running Tests

```bash
# GixSQL integration tests (requires gixpp installed)
./run_tests.sh

# Multi-preparser chaining tests
./run_chain_tests.sh
```

---

## Troubleshooting

**"No such file or directory" error on config:**
- Ensure the path to your `.conf` file is correct
- Or place it in `$COB_CONFIG_DIR` and use `--preparser=<name>` (without path/extension)

**Preparser exits with non-zero:**
- Check that the `command` in your `.conf` is executable
- Test the wrapper script manually: `./gixpp-wrapper.sh input.cob output.cob`
- Set `on-error: warn` to continue compilation even if the preparser fails

**Include files not found by preparser:**
- Pass `-I /path/to/copies` on the `cobc` command line
- The paths are forwarded as the third argument to your wrapper script

**Verbose mode for debugging:**
- Use `cobc -v` to see which preparsers are invoked and with what arguments
