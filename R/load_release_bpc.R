# Functions for downloading and loading a single BPC release into DuckDB.

BPC_SKIP_PATTERNS <- c(
  "^meta_",
  "\\.pdf$",
  "\\.html$"
)

.bpc_is_relevant <- function(name) {
  !any(sapply(BPC_SKIP_PATTERNS, grepl, x = name))
}

BPC_TABLE_MAP <- c(
  "patient_level_dataset"              = "pt",
  "cancer_level_dataset_index"         = "ca_ind",
  "cancer_level_dataset_non_index"     = "ca_non_ind",
  "cancer_panel_test_level_dataset"    = "cpt",
  "regimen_cancer_level_dataset"       = "reg",
  "imaging_level_dataset"              = "img",
  "med_onc_note_level_dataset"         = "med_onc",
  "pathology_report_level_dataset"     = "path"
)

#' Map a BPC data filename to a table name.
#'
#' Uses an explicit mapping. Returns NULL for unrecognized files.
.bpc_table_name <- function(filename) {
  stem <- sub("\\.[^.]+$", "", tolower(filename))
  unname(BPC_TABLE_MAP[stem])
}

#' Load a single BPC release (one cancer type + version) into DuckDB.
#'
#' Downloads files from Synapse, loads each into the appropriate table
#' with bpc_cancer and release columns added. Skips if already loaded.
#'
#' @param con          Open DBI connection to the BPC DuckDB database.
#' @param bpc_cancer   Cancer type string, e.g. "NSCLC".
#' @param release_name Release version string, e.g. "3.1-consortium".
#' @param release_syn_id Synapse folder ID for this release.
#' @param tmp_dir      Directory for temporary file downloads.
#' @return Invisibly TRUE if loaded, FALSE if skipped.
add_bpc_release <- function(
  con,
  bpc_cancer,
  release_name,
  release_syn_id,
  tmp_dir = tempdir()
) {
  # Skip sensitive, archived, or non-standard release names
  if (grepl("sensitive|archived", release_name, ignore.case = TRUE)) {
    message("  [skip] excluded release: ", bpc_cancer, " ", release_name)
    return(invisible(FALSE))
  }

  already <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM releases WHERE bpc_cancer = ? AND release = ?",
    list(bpc_cancer, release_name)
  )[[1]]
  if (already > 0) {
    message("  [skip] already loaded: ", bpc_cancer, " ", release_name)
    return(invisible(FALSE))
  }

  release_tmp <- file.path(tmp_dir, bpc_cancer, release_name)
  dir.create(release_tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(release_tmp, recursive = TRUE), add = TRUE)

  # Find the clinical_data subfolder within the release
  release_children <- as.list(synapser::synGetChildren(release_syn_id))
  clin_folder <- Filter(
    function(c) c$type == "org.sagebionetworks.repo.model.Folder" &&
      grepl("clinical_data", c$name, ignore.case = TRUE),
    release_children
  )
  if (length(clin_folder) == 0) {
    message("  [skip] no clinical_data folder found in: ", bpc_cancer, " ", release_name)
    return(invisible(FALSE))
  }

  children <- as.list(synapser::synGetChildren(clin_folder[[1]]$id))
  paths <- character(0)

  for (child in children) {
    if (child$type == "org.sagebionetworks.repo.model.Folder") next
    if (!.bpc_is_relevant(child$name)) next

    message("    [download] ", child$name)
    tryCatch(
      {
        synapser::synGet(
          child$id,
          downloadLocation = release_tmp,
          followLink = TRUE,
          ifcollision = "overwrite.local"
        )
        p <- file.path(release_tmp, child$name)
        if (file.exists(p)) paths <- c(paths, p)
      },
      error = function(e) {
        message("    [error] downloading ", child$name, ": ", conditionMessage(e))
      }
    )
  }

  message("  Loading ", length(paths), " files...")
  errors <- 0L
  for (path in paths) {
    tryCatch(
      .bpc_load_file(con, bpc_cancer, release_name, path),
      error = function(e) {
        message("    [error] loading ", basename(path), ": ", conditionMessage(e))
        errors <<- errors + 1L
      }
    )
  }

  DBI::dbExecute(
    con,
    "INSERT INTO releases VALUES (?, ?, ?, current_timestamp)",
    list(bpc_cancer, release_name, release_syn_id)
  )

  msg <- sprintf("  [done] %s %s", bpc_cancer, release_name)
  if (errors > 0) msg <- paste0(msg, sprintf(" (%d file errors)", errors))
  message(msg)
  invisible(TRUE)
}

# ---- Internal ----

.bpc_load_file <- function(con, bpc_cancer, release_name, path) {
  name <- basename(path)
  tbl_name <- .bpc_table_name(name)
  if (is.na(tbl_name) || is.null(tbl_name)) {
    message("    [skip] unrecognized: ", name)
    return(invisible(NULL))
  }
  message("    loading: ", name, " -> ", tbl_name)

  escaped <- gsub("'", "''", normalizePath(path))
  read_sql <- sprintf(
    "SELECT '%s' AS bpc_cancer, '%s' AS release, *
     FROM read_csv_auto('%s', header=true, all_varchar=true, nullstr=['', '.'], sample_size=-1)",
    bpc_cancer, release_name, escaped
  )

  if (!DBI::dbExistsTable(con, tbl_name)) {
    DBI::dbExecute(con, sprintf("CREATE TABLE \"%s\" AS %s", tbl_name, read_sql))
    return(invisible(NULL))
  }

  # Add any new columns before inserting
  raw <- readLines(path, n = 20)
  header <- strsplit(raw[!startsWith(raw, "#")][1], ",")[[1]]
  file_cols <- c("bpc_cancer", "release", header)
  table_cols <- DBI::dbGetQuery(
    con,
    sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", tbl_name)
  )$column_name

  for (col in setdiff(tolower(file_cols), tolower(table_cols))) {
    actual <- file_cols[tolower(file_cols) == col][1]
    DBI::dbExecute(
      con,
      sprintf('ALTER TABLE "%s" ADD COLUMN IF NOT EXISTS "%s" VARCHAR', tbl_name, actual)
    )
  }

  DBI::dbExecute(con, sprintf('INSERT INTO "%s" BY NAME %s', tbl_name, read_sql))
}
