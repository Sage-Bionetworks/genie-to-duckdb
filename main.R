# Main entry point for building the GENIE DuckDB database.
#
# Steps:
#   1. Download release files from Synapse
#   2. Build the DuckDB database from downloaded files
#
# Prerequisites:
#   - Synapse account with access to GENIE (syn7492881)
#   - R packages: synapser, duckdb, DBI, data.table, optparse

# 1. Download all GENIE release files from Synapse
source("scripts/get_release_files.R")

# 2. Build the database (downloads from Synapse, loads into DuckDB,
#    normalizes column names). Safe to re-run — already-loaded releases
#    are skipped.
source("scripts/run_batches.R")
