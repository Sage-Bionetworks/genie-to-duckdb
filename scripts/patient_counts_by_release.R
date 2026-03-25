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
    n_pts = n_distinct(patient_id),
    .groups = 'drop'
  ) |>
  mutate(ratio = n_rows / n_pts) |>
  arrange(release_version) |>
  collect() |>
  print(n = Inf)

DBI::dbGetQuery(con, "DESCRIBE releases")

DBI::dbGetQuery(con, "SELECT * from releases")

DBI::dbGetQuery(con, "DESCRIBE mutations")

tbl(con, "mutations") |>
  group_by(release_version) %>%
  summarize(
    n_rows = n(),
    n_samp_with_mut = n_distinct(tumor_sample_barcode),
    .groups = 'drop'
  ) |>
  arrange(release_version) %>%
  print(n = Inf)
# So mutations also doesn't have all the releases for some reason - surprised by that.

DBI::dbDisconnect(con, shutdown = TRUE)

# bpc look:
con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "db/genie_bpc.duckdb",
  read_only = TRUE
)
DBI::dbGetQuery(con, "SHOW TABLES")
# This is convenience we have with duckDB:
DBI::dbGetQuery(con, "DESCRIBE ca_ind")

tbl(con, "ca_ind") |>
  group_by(bpc_cancer) %>%
  summarize(
    n_rows = n(),
    n_pts = n_distinct(record_id),
    .groups = 'drop'
  ) |>
  mutate(ratio = n_rows / n_pts) |>
  collect() |>
  print(n = Inf)
