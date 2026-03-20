#!/usr/bin/env Rscript
#
# Build a DuckDB database from all GENIE Synapse releases.
# For each release: download files → load into DuckDB → delete files.
#
# Usage:
#   Rscript R/build_database.R [--db genie.duckdb] [--tmp /tmp/genie] [--releases 14.0-public,15.1-consortium]

suppressPackageStartupMessages({
  library(synapser)
  library(duckdb)
  library(DBI)
  library(data.table)
  library(optparse)
})

RELEASES_SYN_ID <- "syn7492881"

SKIP_PATTERNS <- c(
  "^data_gene_panel_[A-Z]",  # individual panel defs, not the sample-panel matrix
  "^meta_",
  "\\.pdf$",
  "\\.html$",
  "\\.csv$",
  "_hg19\\.seg$|data_cna.*\\.seg$|genie_data_cna",  # seg files not needed
  "^tmb\\.tsv$"                                      # tmb not needed
)

is_relevant <- function(name) !any(sapply(SKIP_PATTERNS, grepl, x = name))

get_children <- function(parent_id) as.list(synGetChildren(parent_id))

# ---- DuckDB helpers ----

db_execute <- function(con, sql, ...) {
  dbExecute(con, sql, ...)
}

# Read a TSV into DuckDB, adding release_version, skipping # comment lines.
# If the table doesn't exist, CREATE it; otherwise evolve schema and INSERT BY NAME.
append_csv <- function(con, table, release_version, path, delim = "\t") {
  path    <- normalizePath(path)
  escaped <- gsub("'", "''", path)

  read_expr <- sprintf(
    "SELECT '%s' AS release_version, * FROM read_csv_auto('%s', delim='%s', comment='#', header=true, all_varchar=false, nullstr=['', '.'], sample_size=-1)",
    release_version, escaped, delim
  )

  if (!dbExistsTable(con, table)) {
    dbExecute(con, sprintf("CREATE TABLE %s AS %s", table, read_expr))
  } else {
    # Evolve schema: add any columns in this file that don't yet exist in the table
    raw_lines  <- readLines(path, n = 20)
    header_line <- raw_lines[!startsWith(raw_lines, "#")][1]
    file_cols   <- c("release_version", strsplit(header_line, "\t")[[1]])

    table_cols <- dbGetQuery(
      con,
      sprintf("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'", table)
    )$column_name

    for (col in setdiff(tolower(file_cols), tolower(table_cols))) {
      actual_col <- file_cols[tolower(file_cols) == col]
      dbExecute(con, sprintf('ALTER TABLE "%s" ADD COLUMN IF NOT EXISTS "%s" VARCHAR', table, actual_col))
    }

    dbExecute(con, sprintf("INSERT INTO %s BY NAME %s", table, read_expr))
  }
}

# Pivot wide CNA matrix to long format in R, then append to DuckDB.
append_cna <- function(con, release_version, path) {
  path <- normalizePath(path)

  wide <- fread(path, sep = "\t", header = TRUE, data.table = TRUE,
                na.strings = c("", "."))
  if (!"Hugo_Symbol" %in% names(wide)) {
    message("    [warn] CNA file missing Hugo_Symbol: ", path)
    return(invisible(NULL))
  }

  long <- melt(wide, id.vars = "Hugo_Symbol",
               variable.name = "Tumor_Sample_Barcode",
               value.name = "CNA_value",
               variable.factor = FALSE)
  long[, release_version := release_version]
  long[, CNA_value := as.integer(CNA_value)]
  long <- long[!is.na(CNA_value) & CNA_value != 0L]

  if (!dbExistsTable(con, "cna")) {
    dbWriteTable(con, "cna", long)
  } else {
    dbAppendTable(con, "cna", long)
  }
}

# Clinical files: early releases have a combined patient+sample file;
# later releases split into _patient and _sample. Detect by presence of SAMPLE_ID.
append_clinical <- function(con, release_version, path) {
  path <- normalizePath(path)
  header <- scan(path, what = character(), nlines = 1, sep = "\t", quiet = TRUE,
                 comment.char = "#")
  # If comment lines exist, header might be empty - skip past them
  if (length(header) == 0) {
    lines <- readLines(path, n = 10)
    header_line <- lines[!startsWith(lines, "#")][1]
    header <- strsplit(header_line, "\t")[[1]]
  }

  has_sample_id  <- "SAMPLE_ID" %in% header
  has_patient_id <- "PATIENT_ID" %in% header

  escaped <- gsub("'", "''", path)
  read_base <- sprintf(
    "read_csv_auto('%s', delim='\\t', comment='#', header=true, all_varchar=false, nullstr='.')",
    escaped
  )

  # Columns that belong to the patient-level table
  patient_cols <- c("PATIENT_ID", "SEX", "PRIMARY_RACE", "ETHNICITY", "CENTER",
                    "INT_CONTACT", "INT_DOD", "YEAR_CONTACT", "DEAD", "YEAR_DEATH",
                    "BIRTH_YEAR", "SECONDARY_RACE", "TERTIARY_RACE")

  if (has_sample_id && has_patient_id) {
    # Combined file (early releases) — load to both tables

    # Sample table: full file
    sample_expr <- sprintf("SELECT '%s' AS release_version, * FROM %s", release_version, read_base)
    if (!dbExistsTable(con, "clinical_sample")) {
      db_execute(con, sprintf("CREATE TABLE clinical_sample AS %s", sample_expr))
    } else {
      db_execute(con, sprintf("INSERT INTO clinical_sample BY NAME %s", sample_expr))
    }

    # Patient table: deduplicate on PATIENT_ID using only patient-level columns present
    available_patient_cols <- intersect(patient_cols, header)
    col_select <- paste(sprintf('"%s"', available_patient_cols), collapse = ", ")
    patient_expr <- sprintf(
      "SELECT '%s' AS release_version, %s FROM %s GROUP BY ALL",
      release_version, col_select, read_base
    )
    if (!dbExistsTable(con, "clinical_patient")) {
      db_execute(con, sprintf("CREATE TABLE clinical_patient AS %s", patient_expr))
    } else {
      db_execute(con, sprintf("INSERT INTO clinical_patient BY NAME %s", patient_expr))
    }

  } else if (has_sample_id) {
    append_csv(con, "clinical_sample", release_version, path)
  } else {
    append_csv(con, "clinical_patient", release_version, path)
  }
}

# ---- File type dispatch ----

load_file <- function(con, release_version, path) {
  name <- basename(path)
  message("    loading: ", name)

  if (grepl("^data_clinical_patient|^data_clinical\\.txt|^data_clinical_supp", name, ignore.case = TRUE)) {
    append_clinical(con, release_version, path)

  } else if (grepl("^data_clinical_sample", name, ignore.case = TRUE)) {
    append_csv(con, "clinical_sample", release_version, path)

  } else if (grepl("^data_mutations_extended", name, ignore.case = TRUE)) {
    append_csv(con, "mutations", release_version, path)

  } else if (grepl("^data_CNA\\.txt$", name, ignore.case = TRUE)) {
    append_cna(con, release_version, path)

  } else if (grepl("^data_(sv|fusions)", name, ignore.case = TRUE)) {
    append_csv(con, "sv", release_version, path)

  } else if (grepl("^data_gene_(panel_)?matrix", name, ignore.case = TRUE)) {
    append_csv(con, "gene_matrix", release_version, path)

  } else if (grepl("^assay_information", name, ignore.case = TRUE)) {
    append_csv(con, "assay_information", release_version, path)

  } else if (grepl("^genomic_information", name, ignore.case = TRUE)) {
    append_csv(con, "genomic_information", release_version, path)

  } else if (grepl("\\.bed$", name, ignore.case = TRUE)) {
    append_csv(con, "bed", release_version, path)

  } else {
    message("    [skip] unrecognized file: ", name)
  }
}

# ---- Release processing ----

release_already_loaded <- function(con, release_version) {
  if (!dbExistsTable(con, "releases")) return(FALSE)
  n <- dbGetQuery(con, "SELECT COUNT(*) FROM releases WHERE release_version = ?",
                  list(release_version))[[1]]
  n > 0
}

record_release <- function(con, release_version, syn_id) {
  if (!dbExistsTable(con, "releases")) {
    db_execute(con, "CREATE TABLE releases (release_version VARCHAR, syn_id VARCHAR, loaded_at TIMESTAMP)")
  }
  db_execute(con, "INSERT INTO releases VALUES (?, ?, current_timestamp)",
             list(release_version, syn_id))
}

process_release <- function(con, version_name, version_syn_id, tmp_dir) {
  if (release_already_loaded(con, version_name)) {
    message("  [skip] already loaded: ", version_name)
    return(invisible(NULL))
  }

  message("  Downloading files for: ", version_name)
  release_tmp <- file.path(tmp_dir, version_name)
  dir.create(release_tmp, recursive = TRUE, showWarnings = FALSE)

  children <- get_children(version_syn_id)
  paths_downloaded <- character(0)

  for (child in children) {
    name      <- child$name
    child_type <- child$type

    if (child_type == "org.sagebionetworks.repo.model.Folder") next
    if (!is_relevant(name)) next

    message("    [download] ", name)
    tryCatch({
      entity <- synGet(child$id, downloadLocation = release_tmp,
                       followLink = TRUE, ifcollision = "overwrite.local")
      local_path <- file.path(release_tmp, name)
      if (file.exists(local_path)) paths_downloaded <- c(paths_downloaded, local_path)
    }, error = function(e) {
      message("    [error] failed to download ", name, ": ", conditionMessage(e))
    })
  }

  message("  Loading ", length(paths_downloaded), " files into DuckDB...")
  errors <- 0L
  for (path in paths_downloaded) {
    tryCatch(
      load_file(con, version_name, path),
      error = function(e) {
        message("    [error] loading ", basename(path), ": ", conditionMessage(e))
        errors <<- errors + 1L
      }
    )
  }
  record_release(con, version_name, version_syn_id)
  message("  [done] ", version_name, if (errors > 0) sprintf(" (%d file errors)", errors) else "")

  # Clean up temp files regardless of outcome
  unlink(release_tmp, recursive = TRUE)
}

# ---- CLI args ----

option_list <- list(
  make_option("--db",       default = "genie.duckdb",
              help = "Path to DuckDB database file [default: genie.duckdb]"),
  make_option("--tmp",      default = file.path(tempdir(), "genie"),
              help = "Temp directory for downloads [default: system temp]"),
  make_option("--releases", default = NULL,
              help = "Comma-separated release versions to limit to")
)
opts     <- parse_args(OptionParser(option_list = option_list))
db_path  <- opts$db
tmp_dir  <- opts$tmp
releases <- if (!is.null(opts$releases)) strsplit(opts$releases, ",")[[1]] else NULL

# ---- Main ----

synLogin(silent = TRUE)

message("Connecting to DuckDB: ", db_path)
con <- dbConnect(duckdb(), dbdir = db_path)
on.exit(dbDisconnect(con, shutdown = TRUE))

message("Listing release groups under ", RELEASES_SYN_ID, "...")
release_groups <- get_children(RELEASES_SYN_ID)

for (group in release_groups) {
  if (group$type != "org.sagebionetworks.repo.model.Folder") next

  versions <- get_children(group$id)
  for (version in versions) {
    if (version$type != "org.sagebionetworks.repo.model.Folder") next

    version_name <- version$name
    if (!is.null(releases) && !(version_name %in% releases)) next

    message("\n=== ", version_name, " ===")
    process_release(con, version_name, version$id, tmp_dir)
  }
}

message("\nAll done. Tables in database:")
print(dbListTables(con))
