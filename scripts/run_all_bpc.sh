#!/bin/bash
# Build the full GENIE BPC DuckDB database, one cancer type at a time.
# Safe to interrupt and re-run — already-loaded releases are skipped.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

Rscript "$SCRIPT_DIR/run_batches_bpc.R" \
  --db "$SCRIPT_DIR/../db/genie_bpc.duckdb" \
  --tmp /tmp/genie_bpc_build
