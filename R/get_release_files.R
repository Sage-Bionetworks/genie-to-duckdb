#!/usr/bin/env Rscript
#
# Download core data files from every GENIE release on Synapse (syn7492881).
#
# Directory layout:
#   data/<release_version>/<filename>
#
# Skipped:
#   - case_lists/ subfolders
#   - data_gene_panel_<INSTITUTION>*.txt  (individual panel definitions)
#   - meta_*.txt
#   - *.pdf, *.html
#   - *.csv (QC/audit files)
#
# Usage:
#   Rscript R/get_release_files.R [--dry-run] [--output-dir data] [--releases 14.0-public 15.1-consortium ...]

suppressPackageStartupMessages({
  library(synapser)
  library(optparse)
})

RELEASES_SYN_ID <- "syn7492881"

SKIP_PATTERNS <- c(
  "^data_gene_panel_[A-Z]",  # individual panel definitions, not the sample-panel matrix
  "^meta_",
  "\\.pdf$",
  "\\.html$",
  "\\.csv$"
)

is_relevant <- function(name) {
  !any(sapply(SKIP_PATTERNS, grepl, x = name))
}

human_size <- function(n_bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  for (unit in units) {
    if (n_bytes < 1024) return(sprintf("%.1f %s", n_bytes, unit))
    n_bytes <- n_bytes / 1024
  }
  sprintf("%.1f PB", n_bytes)
}

get_children <- function(parent_id) {
  as.list(synGetChildren(parent_id))
}

download_release <- function(version_name, version_syn_id, output_dir, dry_run) {
  release_dir <- file.path(output_dir, version_name)
  children <- get_children(version_syn_id)

  files_downloaded <- 0L
  bytes_to_download <- 0

  for (child in children) {
    name <- child$name
    child_type <- child$type

    # skip subfolders (case_lists, etc.)
    if (child_type == "org.sagebionetworks.repo.model.Folder") {
      message("  [skip folder] ", name, "/")
      next
    }

    if (!is_relevant(name)) {
      message("  [skip file]   ", name)
      next
    }

    dest <- file.path(release_dir, name)
    if (file.exists(dest)) {
      size <- file.info(dest)$size
      message("  [exists]      ", name, " (", human_size(size), ")")
      files_downloaded <- files_downloaded + 1L
      next
    }

    if (dry_run) {
      entity <- synGet(child$id, downloadFile = FALSE)
      size <- tryCatch({
        fh <- reticulate::py_to_r(entity$`_file_handle`)
        as.numeric(fh[["contentSize"]])
      }, error = function(e) 0)
      bytes_to_download <- bytes_to_download + size
      size_str <- if (size > 0) human_size(size) else "unknown size"
      message("  [download]    ", name, " (", size_str, ")")
    } else {
      message("  [download]    ", name)
      dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
      synGet(child$id, downloadLocation = release_dir, ifcollision = "overwrite.local")
    }
    files_downloaded <- files_downloaded + 1L
  }

  list(files = files_downloaded, bytes = bytes_to_download)
}

# --- CLI args ---
option_list <- list(
  make_option("--output-dir", default = "data", help = "Root directory for downloads [default: ./data]"),
  make_option("--dry-run", action = "store_true", default = FALSE, help = "Print what would be downloaded without downloading"),
  make_option("--releases", default = NULL, help = "Comma-separated release versions to limit to, e.g. '14.0-public,15.1-consortium'")
)
opts <- parse_args(OptionParser(option_list = option_list))

output_dir  <- opts[["output-dir"]]
dry_run     <- opts[["dry-run"]]
releases    <- if (!is.null(opts$releases)) strsplit(opts$releases, ",")[[1]] else NULL

# --- Main ---
synLogin(silent = TRUE)

message("Listing release groups under ", RELEASES_SYN_ID, "...")
release_groups <- get_children(RELEASES_SYN_ID)

total_files <- 0L
total_bytes <- 0

for (group in release_groups) {
  if (group$type != "org.sagebionetworks.repo.model.Folder") next

  versions <- get_children(group$id)
  for (version in versions) {
    if (version$type != "org.sagebionetworks.repo.model.Folder") next

    version_name <- version$name
    if (!is.null(releases) && !(version_name %in% releases)) next

    message("\n=== ", version_name, " (", version$id, ") ===")
    result <- download_release(version_name, version$id, output_dir, dry_run)
    total_files <- total_files + result$files
    total_bytes <- total_bytes + result$bytes
  }
}

if (dry_run) {
  message("\nDry run complete: ", total_files, " files, ", human_size(total_bytes), " to download.")
} else {
  message("\nDone. Downloaded ", total_files, " files.")
}
