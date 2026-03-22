# Main entry point for building the GENIE DuckDB databases.
#
# Steps:
#   1. Download GENIE release files from Synapse
#   2. Build the main GENIE database (db/genie.duckdb)
#   3. Build the GENIE BPC database (db/genie_bpc.duckdb)
#
# Prerequisites:
#   - Synapse account with access to GENIE (syn7492881) and BPC (syn21241322)
#   - R packages: synapser, duckdb, DBI, data.table, optparse

# 1. Download all GENIE release files from Synapse
source("scripts/get_release_files.R")

# 2. Build the main GENIE database (downloads from Synapse, loads into DuckDB,
#    normalizes column names). Safe to re-run — already-loaded releases
#    are skipped.
source("scripts/run_batches.R")

# 3. Build the GENIE BPC database (syn21241322). Cancer types (NSCLC, PANC,
#    etc.) and releases are added as columns to each table.
source("scripts/run_batches_bpc.R")
