# Functions for creating and managing the GENIE BPC DuckDB database.

#' Create the BPC database and initialize the releases tracking table.
#'
#' Data tables are created dynamically on first insert so their schemas
#' evolve naturally across releases. Safe to call on an existing database.
#'
#' @param db_path Path to the DuckDB file to create or connect to.
#' @return An open DBI connection (caller is responsible for closing it).
create_bpc_db <- function(db_path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS releases (
      bpc_cancer VARCHAR,
      release    VARCHAR,
      syn_id     VARCHAR,
      loaded_at  TIMESTAMP,
      PRIMARY KEY (bpc_cancer, release)
    )
  ")

  message("BPC database ready: ", db_path)
  con
}
