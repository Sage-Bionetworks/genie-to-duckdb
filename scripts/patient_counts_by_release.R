#!/usr/bin/env Rscript
#
# Count record_ids per release in the patient table.

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(dplyr, warn.conflicts = FALSE)
  library(dbplyr, warn.conflicts = FALSE)
})

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "db/genie.duckdb",
  read_only = TRUE
)

# Show all tables in the database
DBI::dbGetQuery(con, "SHOW TABLES")

# This is convenience we have with duckDB:
DBI::dbGetQuery(con, "DESCRIBE clinical_patient")
# Apparently this should do something similar:
DBI::dbGetQuery(
  con,
  "SELECT column_name, data_type
  FROM information_schema.columns
  WHERE table_name = 'clinical_patient'"
)

tbl(con, "clinical_patient") |>
  group_by(release_version) %>%
  summarize(
    n_rows = n(),
    n_pts = n_distinct(PATIENT_ID),
    .groups = 'drop'
  ) |>
  mutate(ratio = n_rows / n_pts) |>
  arrange(release_version) |>
  collect() |>
  print(n = Inf)

DBI::dbDisconnect(con, shutdown = TRUE)
