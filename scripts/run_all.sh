#!/bin/bash
# Build the full GENIE DuckDB database, one major release group at a time.
# Safe to interrupt and re-run — already-loaded releases are skipped.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

Rscript "$SCRIPT_DIR/run_batches.R" \
  --db "$SCRIPT_DIR/../db/genie.duckdb" \
  --tmp /tmp/genie_build
