#!/usr/bin/env Rscript
#
# Run build_database.R one major release group at a time, newest first.
# Safe to interrupt and re-run — already-loaded releases are skipped.
#
# Usage:
#   Rscript scripts/run_batches.R [--db db/genie.duckdb] [--tmp /tmp/genie]

suppressPackageStartupMessages({
  library(synapser)
  library(duckdb)
  library(DBI)
  library(optparse)
})

r_dir <- file.path(dirname(normalizePath(
  sub("--file=", "", grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
)), "../R")

source(file.path(r_dir, "db.R"))
source(file.path(r_dir, "load_release.R"))

RELEASES_SYN_ID <- "syn7492881"

option_list <- list(
  make_option("--db",  default = "db/genie.duckdb",
              help = "Path to DuckDB database file [default: db/genie.duckdb]"),
  make_option("--tmp", default = file.path(tempdir(), "genie"),
              help = "Temp directory for downloads [default: system temp]")
)
opts    <- parse_args(OptionParser(option_list = option_list))
db_path <- opts$db
tmp_dir <- opts$tmp

synLogin(silent = TRUE)

con <- create_genie_db(db_path)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

message("Fetching release groups from Synapse...")
groups   <- rev(as.list(synGetChildren(RELEASES_SYN_ID)))  # newest first
n_groups <- sum(sapply(groups, function(g) g$type == "org.sagebionetworks.repo.model.Folder"))
i <- 0L

for (group in groups) {
  if (group$type != "org.sagebionetworks.repo.model.Folder") next
  i <- i + 1L

  versions <- Filter(
    function(v) v$type == "org.sagebionetworks.repo.model.Folder",
    as.list(synGetChildren(group$id))
  )

  message(sprintf("\n[%d/%d] %s (%d versions)", i, n_groups, group$name, length(versions)))

  for (version in versions) {
    add_release(con, version$name, version$id, tmp_dir)
  }
}

message("\nAll release groups processed. Tables: ", paste(dbListTables(con), collapse = ", "))
