#!/usr/bin/env Rscript
#
# Build the GENIE DuckDB database, one release at a time.
# Already-loaded releases are skipped automatically — safe to re-run.
#
# Usage:
#   Rscript scripts/build_database.R [--db db/genie.duckdb] [--tmp /tmp/genie] [--releases 14.0-public,15.1-consortium]

suppressPackageStartupMessages({
  library(synapser)
  library(duckdb)
  library(DBI)
  library(data.table)
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
source(file.path(r_dir, "load_release.R"))

RELEASES_SYN_ID <- "syn7492881"

option_list <- list(
  make_option(
    "--db",
    default = "db/genie.duckdb",
    help = "Path to DuckDB database file [default: db/genie.duckdb]"
  ),
  make_option(
    "--tmp",
    default = file.path(tempdir(), "genie"),
    help = "Temp directory for downloads [default: system temp]"
  ),
  make_option(
    "--releases",
    default = NULL,
    help = "Comma-separated release versions to process"
  )
)
opts <- parse_args(OptionParser(option_list = option_list))
db_path <- opts$db
tmp_dir <- opts$tmp
releases <- if (!is.null(opts$releases)) {
  strsplit(opts$releases, ",")[[1]]
} else {
  NULL
}

synLogin(silent = TRUE)

con <- create_genie_db(db_path)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

groups <- as.list(synGetChildren(RELEASES_SYN_ID))
for (group in groups) {
  if (group$type != "org.sagebionetworks.repo.model.Folder") {
    next
  }

  versions <- as.list(synGetChildren(group$id))
  for (version in versions) {
    if (version$type != "org.sagebionetworks.repo.model.Folder") {
      next
    }
    if (!is.null(releases) && !(version$name %in% releases)) {
      next
    }

    message("\n=== ", version$name, " ===")
    add_release(con, version$name, version$id, tmp_dir)
  }
}

message("\nDone. Tables: ", paste(dbListTables(con), collapse = ", "))
