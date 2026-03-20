#!/usr/bin/env Rscript
#
# Run build_database.R one major release group at a time, newest first.
# Safe to interrupt and re-run — already-loaded releases are skipped automatically.
#
# Usage:
#   Rscript scripts/run_batches.R [--db genie.duckdb] [--tmp /tmp/genie]

suppressPackageStartupMessages({
  library(synapser)
  library(duckdb)
  library(DBI)
  library(optparse)
})

RELEASES_SYN_ID <- "syn7492881"
SCRIPT_PATH <- file.path(
  dirname(normalizePath(sub(
    "--file=",
    "",
    grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  ))),
  "build_database.R"
)

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
  )
)
opts <- parse_args(OptionParser(option_list = option_list))
db_path <- opts$db
tmp_dir <- opts$tmp

already_loaded <- function(db_path, versions) {
  if (!file.exists(db_path)) {
    return(character(0))
  }
  tryCatch(
    {
      con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
      on.exit(dbDisconnect(con, shutdown = TRUE))
      if (!dbExistsTable(con, "releases")) {
        return(character(0))
      }
      loaded <- dbGetQuery(
        con,
        "SELECT release_version FROM releases"
      )$release_version
      intersect(versions, loaded)
    },
    error = function(e) {
      message("  [warn] could not read releases table: ", conditionMessage(e))
      character(0)
    }
  )
}

synLogin(silent = TRUE)

message("Fetching release groups from Synapse...")
groups <- rev(as.list(synGetChildren(RELEASES_SYN_ID))) # newest first

n_groups <- sum(sapply(groups, function(g) {
  g$type == "org.sagebionetworks.repo.model.Folder"
}))
i <- 0L

for (group in groups) {
  if (group$type != "org.sagebionetworks.repo.model.Folder") {
    next
  }
  i <- i + 1L

  group_name <- group$name
  versions <- as.list(synGetChildren(group$id))
  version_names <- sapply(
    Filter(
      function(v) v$type == "org.sagebionetworks.repo.model.Folder",
      versions
    ),
    function(v) v$name
  )

  loaded <- already_loaded(db_path, version_names)
  pending <- setdiff(version_names, loaded)

  message(sprintf(
    "\n[%d/%d] %s — %d versions (%d pending, %d already loaded)",
    i,
    n_groups,
    group_name,
    length(version_names),
    length(pending),
    length(loaded)
  ))

  if (length(pending) == 0) {
    message("  All versions loaded, skipping.")
    next
  }

  releases_arg <- paste(pending, collapse = ",")
  cmd <- sprintf(
    'Rscript "%s" --db "%s" --tmp "%s" --releases "%s"',
    SCRIPT_PATH,
    db_path,
    tmp_dir,
    releases_arg
  )

  message("  Running: ", group_name, " (", paste(pending, collapse = ", "), ")")
  ret <- system(cmd)

  if (ret != 0) {
    message(
      "  [warn] build_database.R exited with code ",
      ret,
      " for ",
      group_name
    )
  } else {
    message("  [done] ", group_name)
  }
}

message("\nAll release groups processed.")
