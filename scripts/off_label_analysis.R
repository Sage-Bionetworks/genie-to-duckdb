#!/usr/bin/env Rscript
#
# Estimate the percentage of drug-cancer uses in the GENIE BPC regimen data
# that are off-label (no FDA approval for that cancer type) or off-guideline.
#
# Approach:
#   1. Extract every individual drug use from the reg table (drugs 1-5 unpivoted).
#   2. Normalise drug names (strip salt forms, synonyms in parentheses).
#   3. Look up each drug-cancer pair against a curated reference table of
#      FDA-approved indications (built from FDA labels and NCCN compendia).
#   4. Classify each use as: on-label, off-label-guideline-supported,
#      off-label-off-guideline, investigational, or unclassifiable.
#   5. Report percentages by cancer type and overall.
#
# Caveats:
#   - This is an *estimate*. True on/off-label status depends on specific
#     histology, biomarkers, line of therapy, and label wording that we
#     cannot fully capture here.
#   - Tissue-agnostic approvals (e.g. pembrolizumab for MSI-H/TMB-H) are
#     counted as on-label for all cancer types, since we cannot determine
#     biomarker status from the regimen table alone.
#   - "Investigational Drug" entries are reported separately.
#   - Some drugs appear with slight name variations; we normalise aggressively.
#
# Usage:
#   Rscript scripts/off_label_analysis.R [--db db/genie_bpc.duckdb]

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(optparse)
})

option_list <- list(
  make_option("--db", default = "db/genie_bpc.duckdb",
              help = "Path to BPC DuckDB [default: db/genie_bpc.duckdb]")
)
opts <- parse_args(OptionParser(option_list = option_list))

# ---- 1. Reference table: FDA-approved indications ----
#
# Each entry maps a normalised drug name to the BPC cancer types for which
# the drug has FDA approval.  A second list captures off-label uses that are
# explicitly supported by NCCN guidelines (category 1/2A).
#
# BPC cancer types: BLADDER, BrCa, CRC, NSCLC, PANC, Prostate, RENAL

on_label <- list(
  # --- Platinum agents ---
  "cisplatin"            = c("BLADDER", "NSCLC"),
  "carboplatin"          = c("NSCLC"),
  "oxaliplatin"          = c("CRC"),


  # --- Taxanes ---
  "paclitaxel"           = c("BrCa", "NSCLC"),
  "docetaxel"            = c("BrCa", "NSCLC", "Prostate"),
  "cabazitaxel"          = c("Prostate"),
  "nabpaclitaxel"        = c("BrCa", "NSCLC", "PANC"),

  # --- Antimetabolites ---
  "fluorouracil"         = c("CRC", "BrCa"),
  "capecitabine"         = c("CRC", "BrCa"),
  "gemcitabine"          = c("NSCLC", "PANC", "BrCa", "BLADDER"),
  "pemetrexed"           = c("NSCLC"),
  "methotrexate"         = c("BrCa"),
  "floxuridine"          = c("CRC"),

  # --- Topoisomerase inhibitors ---
  "irinotecan"           = c("CRC", "PANC"),
  "irinotecan liposome"  = c("PANC"),
  "topotecan"            = c(),
  "etoposide"            = c("NSCLC"),

  # --- Anthracyclines ---
  "doxorubicin"          = c("BrCa", "BLADDER"),
  "epirubicin"           = c("BrCa"),
  "pegylated liposomal doxorubicin" = c(),
  "mitoxantrone"         = c("Prostate"),
  "valrubicin"           = c("BLADDER"),

  # --- Alkylating agents ---
  "cyclophosphamide"     = c("BrCa"),
  "ifosfamide"           = c(),
  "temozolomide"         = c(),
  "carmustine"           = c(),
  "thiotepa"             = c("BrCa", "BLADDER"),

  # --- Other cytotoxics ---
  "bleomycin"            = c(),
  "vinorelbine"          = c("BrCa", "NSCLC"),
  "vinblastine"          = c("BLADDER"),
  "vincristine"          = c(),
  "eribulin"             = c("BrCa"),
  "ixabepilone"          = c("BrCa"),
  "mitomycin"            = c("BLADDER"),
  "estramustine"         = c("Prostate"),
  "lurbinectedin"        = c("NSCLC"),

  # --- Checkpoint inhibitors ---
  "pembrolizumab"        = c("NSCLC", "BLADDER", "BrCa", "CRC", "RENAL",
                              "PANC", "Prostate"),
  # tissue-agnostic MSI-H/TMB-H; counted as on-label everywhere
  "nivolumab"            = c("NSCLC", "BLADDER", "CRC", "RENAL"),
  "atezolizumab"         = c("NSCLC", "BLADDER", "BrCa"),
  "durvalumab"           = c("NSCLC", "BLADDER"),
  "avelumab"             = c("BLADDER", "RENAL"),
  "ipilimumab"           = c("RENAL", "NSCLC", "CRC"),
  "tremelimumab"         = c("NSCLC"),

  # --- Anti-HER2 ---
  "trastuzumab"          = c("BrCa"),
  "pertuzumab"           = c("BrCa"),
  "pertuzumab-trastuzumab-hyaluronidase-zzxf" = c("BrCa"),
  "trastuzumab emtansine" = c("BrCa"),
  "trastuzumab deruxtecan" = c("BrCa", "NSCLC", "CRC"),
  "trastuzumab/hyaluronidase-oysk" = c("BrCa"),
  "tucatinib"            = c("BrCa", "CRC"),
  "neratinib"            = c("BrCa"),
  "lapatinib"            = c("BrCa"),

  # --- Anti-VEGF / antiangiogenic ---
  "bevacizumab"          = c("CRC", "NSCLC", "RENAL"),
  "ramucirumab"          = c("CRC", "NSCLC"),
  "ziv aflibercept"      = c("CRC"),
  "regorafenib"          = c("CRC"),
  "trifluridine and tipiracil" = c("CRC"),

  # --- EGFR antibodies ---
  "cetuximab"            = c("CRC"),
  "panitumumab"          = c("CRC"),
  "necitumumab"          = c("NSCLC"),

  # --- Hormonal therapy (breast) ---
  "tamoxifen"            = c("BrCa"),
  "letrozole"            = c("BrCa"),
  "fulvestrant"          = c("BrCa"),
  "anastrozole"          = c("BrCa"),
  "exemestane"           = c("BrCa"),
  "toremifene"           = c("BrCa"),
  "megestrol"            = c("BrCa"),
  "raloxifene"           = c("BrCa"),

  # --- CDK4/6 inhibitors ---
  "palbociclib"          = c("BrCa"),
  "ribociclib"           = c("BrCa"),
  "abemaciclib"          = c("BrCa"),

  # --- PI3K / mTOR ---
  "alpelisib"            = c("BrCa"),
  "everolimus"           = c("BrCa", "RENAL"),
  "temsirolimus"         = c("RENAL"),

  # --- PARP inhibitors ---
  "olaparib"             = c("BrCa", "Prostate", "PANC"),
  "rucaparib"            = c("Prostate"),
  "talazoparib"          = c("BrCa"),
  "niraparib"            = c(),

  # --- Hormonal therapy (prostate) ---
  "bicalutamide"         = c("Prostate"),
  "leuprolide"           = c("Prostate", "BrCa"),
  "enzalutamide"         = c("Prostate"),
  "degarelix"            = c("Prostate"),
  "abiraterone"          = c("Prostate"),
  "apalutamide"          = c("Prostate"),
  "darolutamide"         = c("Prostate"),
  "flutamide"            = c("Prostate"),
  "nilutamide"           = c("Prostate"),
  "triptorelin"          = c("Prostate"),
  "histrelin"            = c("Prostate"),
  "goserelin"            = c("Prostate", "BrCa"),
  "adt lhrh agonist not specified" = c("Prostate"),

  # --- Prostate other ---
  "sipuleucel t"         = c("Prostate"),
  "radium ra 223 dichloride" = c("Prostate"),

  # --- NSCLC targeted ---
  "osimertinib"          = c("NSCLC"),
  "erlotinib"            = c("NSCLC", "PANC"),
  "gefitinib"            = c("NSCLC"),
  "afatinib"             = c("NSCLC"),
  "dacomitinib"          = c("NSCLC"),
  "crizotinib"           = c("NSCLC"),
  "alectinib"            = c("NSCLC"),
  "ceritinib"            = c("NSCLC"),
  "brigatinib"           = c("NSCLC"),
  "lorlatinib"           = c("NSCLC"),
  "capmatinib"           = c("NSCLC"),
  "tepotinib"            = c("NSCLC"),
  "selpercatinib"        = c("NSCLC"),
  "pralsetinib"          = c("NSCLC"),
  "sotorasib"            = c("NSCLC"),
  "amivantamab"          = c("NSCLC"),

  # --- BRAF/MEK ---
  "dabrafenib"           = c("NSCLC"),
  "trametinib"           = c("NSCLC"),
  "encorafenib"          = c("CRC"),
  "binimetinib"          = c(),
  "cobimetinib"          = c(),
  "vemurafenib"          = c(),

  # --- Renal targeted ---
  "sunitinib"            = c("RENAL"),
  "pazopanib"            = c("RENAL"),
  "axitinib"             = c("RENAL"),
  "cabozantinib"         = c("RENAL"),
  "lenvatinib"           = c("RENAL"),
  "tivozanib"            = c("RENAL"),
  "belzutifan"           = c("RENAL"),
  "sorafenib"            = c("RENAL"),

  # --- Bladder specific ---
  "bcg vaccine"          = c("BLADDER"),
  "bcg solution"         = c("BLADDER"),
  "erdafitinib"          = c("BLADDER"),
  "enfortumab vedotin"   = c("BLADDER"),
  "sacituzumab govitecan" = c("BLADDER", "BrCa"),

  # --- Tissue-agnostic ---
  "entrectinib"          = c("BLADDER", "BrCa", "CRC", "NSCLC", "PANC",
                              "Prostate", "RENAL"),
  "larotrectinib"        = c("BLADDER", "BrCa", "CRC", "NSCLC", "PANC",
                              "Prostate", "RENAL"),

  # --- Interferons / immunotherapy ---
  "aldesleukin"          = c("RENAL"),
  "interferon"           = c("RENAL"),
  "recombinant interferon alfa" = c("RENAL"),
  "recombinant interferon alfa2a" = c("RENAL"),
  "peginterferon alfa2b" = c(),

  # --- Other targeted ---
  "olaratumab"           = c(),
  "leucovorin"           = c("CRC"),
  "leucovorin calcium"   = c("CRC"),

  # --- Misc targeted/other ---
  "imatinib"             = c(),
  "rituximab"            = c(),
  "brentuximab vedotin"  = c(),
  "lenalidomide"         = c(),
  "bortezomib"           = c(),
  "dacarbazine"          = c(),
  "ifosfamide"           = c(),
  "temozolomide"         = c(),
  "bendamustine"         = c(),
  "fludarabine"          = c(),
  "hydroxyurea"          = c(),
  "melphalan"            = c(),
  "procarbazine"         = c(),
  "dactinomycin"         = c(),
  "busulfan"             = c(),
  "lomustine"            = c(),
  "cladribine"           = c(),
  "chlorambucil"         = c(),
  "lutetium lu 177 dotatate" = c("PANC"),
  "tegafurgimeraciloteracil" = c("CRC"),
  "pioglitazone"         = c(),
  "octreotide"           = c(),
  "lanreotide"           = c("PANC"),
  "vedolizumab"          = c(),
  "methoxsalen"          = c(),
  "tretinoin"            = c(),
  "arsenic trioxide"     = c(),
  "azacitidine"          = c(),
  "decitabine"           = c(),
  "gilteritinib"         = c(),
  "ivosidenib"           = c(),
  "ripretinib"           = c(),
  "talimogene laherparepvec" = c(),
  "navitoclax"           = c(),
  "onalespib"            = c(),
  "onvansertib"          = c(),
  "ensartinib"           = c(),
  "icotinib"             = c(),
  "rociletinib"          = c(),
  "taselisib"            = c(),
  "apatinib"             = c(),
  "acalabrutinib"        = c(),
  "ibrutinib"            = c(),
  "venetoclax"           = c(),
  "bosutinib"            = c(),
  "dasatinib"            = c(),
  "nilotinib"            = c(),
  "ponatinib"            = c(),
  "ruxolitinib"          = c(),
  "thalidomide"          = c(),
  "pomalidomide"         = c(),
  "carfilzomib"          = c(),
  "ixazomib"             = c(),
  "daratumumab"          = c(),
  "elotuzumab"           = c(),
  "obinutuzumab"         = c(),
  "blinatumomab"         = c(),
  "cytarabine"           = c(),
  "cytarabine liposomal" = c(),
  "daunorubicin"         = c(),
  "liposome daunorubicin cytarabine" = c(),
  "mechlorethamine"      = c(),
  "iodine i-131"         = c(),
  "iodine i 131 tositumomab" = c(),
  "yttrium y90 ibritumomab tiuxetan" = c(),
  "autologous melanoma lysate pulsed dendritic cell vaccine" = c(),
  "interferon alfacon1"  = c(),
  "paclitaxel loaded polymeric micelle" = c("BrCa", "NSCLC"),
  "paclitaxel poliglumex" = c(),
  "paclitaxel trevatide"  = c(),
  "mitotane"             = c(),
  "asparaginase"         = c()
)

# Off-label but NCCN guideline-supported uses (category 1 or 2A)
off_label_guideline <- list(
  "carboplatin"          = c("BLADDER", "BrCa", "Prostate"),
  "oxaliplatin"          = c("PANC"),
  "cisplatin"            = c("PANC", "Prostate"),
  "paclitaxel"           = c("BLADDER"),
  "docetaxel"            = c("BLADDER"),
  "fluorouracil"         = c("PANC"),
  "capecitabine"         = c("PANC"),
  "irinotecan"           = c("NSCLC"),
  "gemcitabine"          = c("CRC"),
  "cyclophosphamide"     = c("NSCLC"),
  "trastuzumab"          = c("CRC"),
  "bevacizumab"          = c("BrCa"),
  "binimetinib"          = c("CRC"),
  "etoposide"            = c("Prostate"),
  "leucovorin"           = c("PANC"),
  "leucovorin calcium"   = c("PANC"),
  "ketoconazole"         = c("Prostate"),
  "dutasteride"          = c("Prostate"),
  "mitomycin"            = c("CRC"),
  "doxorubicin"          = c("NSCLC"),
  "pegylated liposomal doxorubicin" = c("BrCa")
)


# ---- 2. Drug name normalisation ----

normalise_drug <- function(name) {
  # Strip everything after first open paren (synonym lists)
  x <- sub("\\(.*", "", name)
  # Some entries use commas instead of parens for synonyms; keep only first term
  # if the part after the first comma looks like a synonym (starts with uppercase)
  if (grepl(",", x)) {
    first <- trimws(sub(",.*", "", x))
    # Only strip if first part looks like a complete drug name (2+ chars)
    if (nchar(first) >= 2) x <- first
  }
  x <- trimws(tolower(x))

  # Normalise salt forms and suffixes
  salt_patterns <- c(
    " hydrochloride$", " hcl$", " hcl ", " mesylate$", " tosylate$",
    " ditosylate$", " dimaleate$", " phosphate$", " citrate$",
    " acetate$", " malate$", " smalate$", " sulfate$", " camsylate$",
    " potassium$", " tartrate$", " disodium$"
  )
  for (pat in salt_patterns) {
    x <- sub(pat, "", x)
  }
  # Second pass for double salts left over
  for (pat in salt_patterns) {
    x <- sub(pat, "", x)
  }

  x <- trimws(x)

  # Map common aliases and compound formulations to base drug names
  aliases <- c(
    "adt/lhrh agonist not specified" = "adt lhrh agonist not specified",
    "goserlin"            = "goserelin",
    "vincristine sulfate liposome" = "vincristine",
    "rituximab and hyaluronidase human" = "rituximab",
    "pegylated liposomal doxorubicin hydrochloride" = "pegylated liposomal doxorubicin",
    "epirubicin hcl"      = "epirubicin",
    "doxorubicin hcl"     = "doxorubicin",
    "erlotinib hcl"       = "erlotinib",
    "irinotecan hcl"      = "irinotecan",
    "gemcitabine hcl"     = "gemcitabine",
    "mitoxantrone hcl"    = "mitoxantrone",
    "topotecan hcl"       = "topotecan",
    "pazopanib hcl"       = "pazopanib",
    "ponatinib hcl"       = "ponatinib",
    "mechlorethamine hcl" = "mechlorethamine",
    "procarbazine hcl"    = "procarbazine",
    "daunorubicin hcl"    = "daunorubicin",
    "nilotinib hydrochloride monohydrate" = "nilotinib"
  )
  if (x %in% names(aliases)) x <- aliases[[x]]

  x
}


# ---- 3. Classification ----

classify_use <- function(drug_norm, cancer) {
  if (drug_norm == "investigational drug") return("investigational")
  if (drug_norm %in% c("other nos", "other antineoplastic", "other hormone"))
    return("unclassifiable")

  in_fda  <- drug_norm %in% names(on_label)
  in_nccn <- drug_norm %in% names(off_label_guideline)

  if (in_fda && cancer %in% on_label[[drug_norm]]) return("on_label")
  if (in_nccn && cancer %in% off_label_guideline[[drug_norm]]) return("off_label_guideline")
  if (in_fda || in_nccn) return("off_label_off_guideline")

  # Drug not in our reference at all
  "unclassifiable"
}


# ---- 4. Run analysis ----

con <- dbConnect(duckdb(), opts$db, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

uses <- dbGetQuery(con, "
  WITH all_drugs AS (
    SELECT bpc_cancer, drugs_drug_1 AS drug FROM reg WHERE drugs_drug_1 IS NOT NULL
    UNION ALL SELECT bpc_cancer, drugs_drug_2 FROM reg WHERE drugs_drug_2 IS NOT NULL
    UNION ALL SELECT bpc_cancer, drugs_drug_3 FROM reg WHERE drugs_drug_3 IS NOT NULL
    UNION ALL SELECT bpc_cancer, drugs_drug_4 FROM reg WHERE drugs_drug_4 IS NOT NULL
    UNION ALL SELECT bpc_cancer, drugs_drug_5 FROM reg WHERE drugs_drug_5 IS NOT NULL
  )
  SELECT bpc_cancer,
         CASE WHEN drug LIKE '%(%' THEN trim(split_part(drug, '(', 1))
              ELSE trim(drug) END AS drug_name
  FROM all_drugs
")

uses$drug_norm <- vapply(uses$drug_name, normalise_drug, character(1))
uses$status <- mapply(classify_use, uses$drug_norm, uses$bpc_cancer)


# ---- 5. Report ----

cat("\n")
cat(strrep("=", 72), "\n")
cat("  GENIE BPC Off-Label / Off-Guideline Drug Use Analysis\n")
cat(strrep("=", 72), "\n\n")

cat("Methodology:\n")
cat("  - Each row = one individual drug use (drugs_drug_1..5 unpivoted)\n")
cat("  - On-label: drug has FDA approval for that specific cancer type\n")
cat("  - Off-label, guideline-supported: no FDA approval but NCCN-recommended\n")
cat("  - Off-label, off-guideline: no FDA approval, not NCCN-recommended\n")
cat("  - Investigational: explicitly coded as 'Investigational Drug'\n")
cat("  - Unclassifiable: drug not in reference table or coded as 'Other'\n")
cat("  - Tissue-agnostic approvals (MSI-H/TMB-H, NTRK) counted as on-label\n")
cat("    for all cancer types (biomarker status unavailable in regimen data)\n\n")

# Overall summary
tab <- table(uses$status)
n_total <- nrow(uses)
cat(sprintf("Total individual drug uses: %s\n\n", format(n_total, big.mark = ",")))

status_labels <- c(
  "on_label"                 = "On-label (FDA-approved)",
  "off_label_guideline"      = "Off-label, guideline-supported (NCCN)",
  "off_label_off_guideline"  = "Off-label AND off-guideline",
  "investigational"          = "Investigational drug",
  "unclassifiable"           = "Unclassifiable (not in reference)"
)

cat("OVERALL SUMMARY\n")
cat(strrep("-", 60), "\n")
for (s in names(status_labels)) {
  n <- if (s %in% names(tab)) tab[[s]] else 0L
  cat(sprintf("  %-45s %6s (%5.1f%%)\n",
              status_labels[[s]], format(n, big.mark = ","),
              100 * n / n_total))
}

# Combined off-label rate (excluding investigational and unclassifiable)
classifiable <- uses[!uses$status %in% c("investigational", "unclassifiable"), ]
n_class <- nrow(classifiable)
n_off <- sum(classifiable$status != "on_label")
cat(sprintf("\n  Among classifiable, non-investigational uses (n=%s):\n",
            format(n_class, big.mark = ",")))
cat(sprintf("    Off-label (any):  %s / %s = %.1f%%\n",
            format(n_off, big.mark = ","),
            format(n_class, big.mark = ","),
            100 * n_off / n_class))

# By cancer type
cat(sprintf("\n\n%-10s %7s %9s %9s %9s %9s %9s  %s\n",
            "Cancer", "Total", "On-label", "OL+Guide", "OL+OG",
            "Investig", "Unclass", "Off-label%"))
cat(strrep("-", 90), "\n")

for (cancer in sort(unique(uses$bpc_cancer))) {
  sub <- uses[uses$bpc_cancer == cancer, ]
  n <- nrow(sub)
  st <- table(factor(sub$status, levels = names(status_labels)))

  cls <- sub[!sub$status %in% c("investigational", "unclassifiable"), ]
  n_cls <- nrow(cls)
  n_off_c <- sum(cls$status != "on_label")
  pct <- if (n_cls > 0) sprintf("%.1f%%", 100 * n_off_c / n_cls) else "N/A"

  cat(sprintf("%-10s %7s %9s %9s %9s %9s %9s  %s\n",
              cancer,
              format(n, big.mark = ","),
              format(st[["on_label"]], big.mark = ","),
              format(st[["off_label_guideline"]], big.mark = ","),
              format(st[["off_label_off_guideline"]], big.mark = ","),
              format(st[["investigational"]], big.mark = ","),
              format(st[["unclassifiable"]], big.mark = ","),
              pct))
}

# Top off-label drugs
cat("\n\nTOP 20 OFF-LABEL DRUG-CANCER COMBINATIONS (by frequency)\n")
cat(strrep("-", 72), "\n")
off <- uses[uses$status %in% c("off_label_guideline", "off_label_off_guideline"), ]
off_tab <- as.data.frame(
  table(off$bpc_cancer, off$drug_norm, off$status),
  stringsAsFactors = FALSE
)
names(off_tab) <- c("cancer", "drug", "status", "n")
off_tab <- off_tab[off_tab$n > 0, ]
off_tab <- off_tab[order(-off_tab$n), ]
off_tab$label <- ifelse(
  off_tab$status == "off_label_guideline",
  "NCCN-supported", "off-guideline"
)

cat(sprintf("%-10s %-35s %6s  %s\n", "Cancer", "Drug", "Uses", "Status"))
cat(strrep("-", 72), "\n")
for (i in seq_len(min(20, nrow(off_tab)))) {
  r <- off_tab[i, ]
  cat(sprintf("%-10s %-35s %6s  %s\n", r$cancer, r$drug, r$n, r$label))
}

# Unclassifiable drugs
unclass <- uses[uses$status == "unclassifiable", ]
if (nrow(unclass) > 0) {
  unclass_tab <- sort(table(unclass$drug_norm), decreasing = TRUE)
  cat(sprintf(
    "\n\nUNCLASSIFIED DRUGS (top 20, %s total uses, %d unique drugs)\n",
    format(nrow(unclass), big.mark = ","), length(unclass_tab)
  ))
  cat(strrep("-", 50), "\n")
  for (i in seq_len(min(20, length(unclass_tab)))) {
    cat(sprintf("  %-40s %5d\n", names(unclass_tab)[i], unclass_tab[i]))
  }
}

cat("\n")
