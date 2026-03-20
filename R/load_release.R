# Functions for downloading and loading a single GENIE release into DuckDB.

library(data.table)

SKIP_PATTERNS <- c(
  "^data_gene_panel_[A-Z]",
  "^meta_",
  "\\.pdf$",
  "\\.html$",
  "\\.csv$",
  "_hg19\\.seg$|data_cna.*\\.seg$|genie_data_cna",
  "^tmb\\.tsv$"
)

.is_relevant <- function(name) !any(sapply(SKIP_PATTERNS, grepl, x = name))

#' Load a single GENIE release into an open DuckDB connection.
#'
#' Downloads files from Synapse, loads each into the appropriate table, then
#' deletes the local files. Skips the release if it is already recorded in the
#' releases table.
#'
#' @param con     Open DBI connection to the GENIE DuckDB database.
#' @param version_name  Release version string, e.g. "14.0-public".
#' @param version_syn_id  Synapse folder ID for this release version.
#' @param tmp_dir Directory to use for temporary file downloads.
#' @return Invisibly TRUE if loaded, FALSE if skipped.
add_release <- function(
  con,
  version_name,
  version_syn_id,
  tmp_dir = tempdir()
) {
  # Skip if already loaded
  already <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM releases WHERE release_version = ?",
    list(version_name)
  )[[1]]
  if (already > 0) {
    message("  [skip] already loaded: ", version_name)
    return(invisible(FALSE))
  }

  release_tmp <- file.path(tmp_dir, version_name)
  dir.create(release_tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(release_tmp, recursive = TRUE), add = TRUE)

  # Download relevant files
  children <- as.list(synapser::synGetChildren(version_syn_id))
  paths <- character(0)

  for (child in children) {
    if (child$type == "org.sagebionetworks.repo.model.Folder") {
      next
    }
    if (!.is_relevant(child$name)) {
      next
    }

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
        message(
          "    [error] downloading ",
          child$name,
          ": ",
          conditionMessage(e)
        )
      }
    )
  }

  # Load each file
  message("  Loading ", length(paths), " files...")
  errors <- 0L
  for (path in paths) {
    tryCatch(
      .load_file(con, version_name, path),
      error = function(e) {
        message(
          "    [error] loading ",
          basename(path),
          ": ",
          conditionMessage(e)
        )
        errors <<- errors + 1L
      }
    )
  }

  # Record release
  DBI::dbExecute(
    con,
    "INSERT INTO releases VALUES (?, ?, current_timestamp)",
    list(version_name, version_syn_id)
  )

  msg <- sprintf("  [done] %s", version_name)
  if (errors > 0) {
    msg <- paste0(msg, sprintf(" (%d file errors)", errors))
  }
  message(msg)
  invisible(TRUE)
}

# ---- Internal file loaders ----

.load_file <- function(con, release_version, path) {
  name <- basename(path)
  message("    loading: ", name)

  if (
    grepl(
      "^data_clinical_patient|^data_clinical\\.txt|^data_clinical_supp",
      name,
      ignore.case = TRUE
    )
  ) {
    .load_clinical(con, release_version, path)
  } else if (grepl("^data_clinical_sample", name, ignore.case = TRUE)) {
    .insert_csv(con, "clinical_sample", release_version, path)
  } else if (grepl("^data_mutations_extended", name, ignore.case = TRUE)) {
    .insert_csv(con, "mutations", release_version, path)
  } else if (grepl("^data_CNA\\.txt$", name, ignore.case = TRUE)) {
    .load_cna(con, release_version, path)
  } else if (grepl("^data_(sv|fusions)", name, ignore.case = TRUE)) {
    .insert_csv(con, "sv", release_version, path)
  } else if (grepl("^data_gene_(panel_)?matrix", name, ignore.case = TRUE)) {
    .insert_csv(con, "gene_matrix", release_version, path)
  } else if (grepl("^assay_information", name, ignore.case = TRUE)) {
    .insert_csv(con, "assay_information", release_version, path)
  } else if (grepl("^genomic_information", name, ignore.case = TRUE)) {
    .insert_csv(con, "genomic_information", release_version, path)
  } else if (grepl("\\.bed$", name, ignore.case = TRUE)) {
    .insert_csv(con, "bed", release_version, path)
  } else {
    message("    [skip] unrecognized: ", name)
  }
}

# Insert a TSV into a table, creating it on first use and adding new columns as needed.
.insert_csv <- function(con, table, release_version, path) {
  escaped  <- gsub("'", "''", normalizePath(path))
  read_sql <- sprintf(
    "SELECT '%s' AS release_version, *
     FROM read_csv_auto('%s', delim='\\t', comment='#', header=true,
                        all_varchar=true, nullstr=['', '.'], sample_size=-1)",
    release_version, escaped
  )

  if (!DBI::dbExistsTable(con, table)) {
    DBI::dbExecute(con, sprintf("CREATE TABLE %s AS %s", table, read_sql))
    return(invisible(NULL))
  }

  # Add any new columns from this file before inserting
  raw        <- readLines(path, n = 20)
  header     <- strsplit(raw[!startsWith(raw, "#")][1], "\t")[[1]]
  file_cols  <- c("release_version", header)
  table_cols <- DBI::dbGetQuery(
    con,
    sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", table)
  )$column_name

  for (col in setdiff(tolower(file_cols), tolower(table_cols))) {
    actual <- file_cols[tolower(file_cols) == col][1]
    DBI::dbExecute(con, sprintf('ALTER TABLE "%s" ADD COLUMN IF NOT EXISTS "%s" VARCHAR', table, actual))
  }

  DBI::dbExecute(con, sprintf("INSERT INTO %s BY NAME %s", table, read_sql))
}

# Clinical files: early releases have a combined patient+sample file.
.load_clinical <- function(con, release_version, path) {
  lines <- readLines(path, n = 20)
  header <- strsplit(lines[!startsWith(lines, "#")][1], "\t")[[1]]

  has_sample <- "SAMPLE_ID" %in% header
  has_patient <- "PATIENT_ID" %in% header

  if (has_sample) {
    .insert_csv(con, "clinical_sample", release_version, path)
  }
  if (has_patient && !has_sample) {
    .insert_csv(con, "clinical_patient", release_version, path)
  }
  if (has_sample && has_patient) {
    # Combined file: also extract patient-level rows
    patient_cols <- c(
      "PATIENT_ID",
      "SEX",
      "PRIMARY_RACE",
      "ETHNICITY",
      "CENTER",
      "INT_CONTACT",
      "INT_DOD",
      "YEAR_CONTACT",
      "DEAD",
      "YEAR_DEATH",
      "BIRTH_YEAR",
      "SECONDARY_RACE",
      "TERTIARY_RACE"
    )
    available <- intersect(patient_cols, header)
    col_sql <- paste(sprintf('"%s"', available), collapse = ", ")
    escaped <- gsub("'", "''", normalizePath(path))
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT INTO clinical_patient BY NAME
       SELECT '%s' AS release_version, %s
       FROM read_csv_auto('%s', delim='\\t', comment='#', header=true,
                          all_varchar=false, nullstr=['', '.'], sample_size=-1)
       GROUP BY ALL",
        release_version,
        col_sql,
        escaped
      )
    )
  }
}

# Pivot wide CNA matrix to long, drop zeros, then insert.
.load_cna <- function(con, release_version, path) {
  wide <- fread(
    path,
    sep = "\t",
    header = TRUE,
    data.table = TRUE,
    na.strings = c("", ".")
  )

  if (!"Hugo_Symbol" %in% names(wide)) {
    message(
      "    [warn] CNA file missing Hugo_Symbol, skipping: ",
      basename(path)
    )
    return(invisible(NULL))
  }

  long <- melt(
    wide,
    id.vars = "Hugo_Symbol",
    variable.name = "Tumor_Sample_Barcode",
    value.name = "CNA_value",
    variable.factor = FALSE
  )
  long[, release_version := release_version]
  long[, CNA_value := suppressWarnings(as.integer(CNA_value))]
  long <- long[!is.na(CNA_value) & CNA_value != 0L]

  DBI::dbAppendTable(con, "cna", long)
}
