# Functions for creating and managing the GENIE DuckDB database.

#' Create the GENIE database and initialize the releases tracking table.
#'
#' Data tables (mutations, cna, etc.) are created dynamically on first insert
#' so their schemas evolve naturally across releases. Safe to call on an
#' existing database.
#'
#' @param db_path Path to the DuckDB file to create or connect to.
#' @return An open DBI connection (caller is responsible for closing it).
create_genie_db <- function(db_path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS releases (
      release_version VARCHAR PRIMARY KEY,
      syn_id          VARCHAR,
      loaded_at       TIMESTAMP
    )
  ")

  # Also pre-create the cna table since its schema is fixed (we build it ourselves)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cna (
      release_version      VARCHAR,
      Hugo_Symbol          VARCHAR,
      Tumor_Sample_Barcode VARCHAR,
      CNA_value            INTEGER
    )
  ")

  message("Database ready: ", db_path)
  con
}
