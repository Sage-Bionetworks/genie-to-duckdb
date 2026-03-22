#!/usr/bin/env Rscript
#
# Build the GENIE BPC DuckDB database.
# Structure: cancer type folders -> release folders -> data files.
# Safe to interrupt and re-run — already-loaded releases are skipped.
#
# Usage:
#   Rscript scripts/run_batches_bpc.R [--db db/genie_bpc.duckdb] [--tmp /tmp/genie_bpc]

suppressPackageStartupMessages({
  library(synapser)
  library(duckdb)
  library(DBI)
  library(optparse)
})

r_dir <- file.path(
  dirname(normalizePath(
    sub(
      "--file=",
      "",
      grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    )
  )),
  "../R"
)

source(file.path(r_dir, "db.R"))
source(file.path(r_dir, "db_bpc.R"))
source(file.path(r_dir, "load_release_bpc.R"))

BPC_SYN_ID <- "syn21241322"

option_list <- list(
  make_option(
    "--db",
    default = "db/genie_bpc.duckdb",
    help = "Path to DuckDB database file [default: db/genie_bpc.duckdb]"
  ),
  make_option(
    "--tmp",
    default = file.path(tempdir(), "genie_bpc"),
    help = "Temp directory for downloads [default: system temp]"
  )
)
opts <- parse_args(OptionParser(option_list = option_list))
db_path <- opts$db
tmp_dir <- opts$tmp

synLogin(silent = TRUE)

con <- create_bpc_db(db_path)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

message("Fetching cancer types from Synapse...")
cancer_types <- Filter(
  function(c) c$type == "org.sagebionetworks.repo.model.Folder",
  as.list(synGetChildren(BPC_SYN_ID))
)
n_cancers <- length(cancer_types)
i <- 0L

for (cancer in cancer_types) {
  # Skip non-BPC folders
  if (grepl("cBioPortal|Main GENIE", cancer$name, ignore.case = TRUE)) next

  i <- i + 1L
  bpc_cancer <- cancer$name

  releases <- Filter(
    function(r) r$type == "org.sagebionetworks.repo.model.Folder",
    as.list(synGetChildren(cancer$id))
  )

  message(sprintf(
    "\n[%d/%d] %s (%d releases)",
    i, n_cancers, bpc_cancer, length(releases)
  ))

  for (rel in releases) {
    add_bpc_release(con, bpc_cancer, rel$name, rel$id, tmp_dir)
  }
}

normalize_column_names(con)

message(
  "\nAll BPC releases processed. Tables: ",
  paste(dbListTables(con), collapse = ", ")
)
