# Functions for creating and managing the GENIE DuckDB database.

#' Create the GENIE database and initialize all tables.
#'
#' Safe to call on an existing database — tables are created only if they don't
#' already exist.
#'
#' @param db_path Path to the DuckDB file to create or connect to.
#' @return An open DBI connection (caller is responsible for closing it).
create_genie_db <- function(db_path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS releases (
      release_version VARCHAR PRIMARY KEY,
      syn_id          VARCHAR,
      loaded_at       TIMESTAMP
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS clinical_patient (
      release_version VARCHAR,
      PATIENT_ID      VARCHAR,
      SEX             VARCHAR,
      PRIMARY_RACE    VARCHAR,
      ETHNICITY       VARCHAR,
      CENTER          VARCHAR,
      INT_CONTACT     DOUBLE,
      INT_DOD         DOUBLE,
      YEAR_CONTACT    DOUBLE,
      DEAD            VARCHAR,
      YEAR_DEATH      DOUBLE,
      BIRTH_YEAR      DOUBLE,
      SECONDARY_RACE  VARCHAR,
      TERTIARY_RACE   VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS clinical_sample (
      release_version      VARCHAR,
      PATIENT_ID           VARCHAR,
      SAMPLE_ID            VARCHAR,
      AGE_AT_SEQ_REPORT    VARCHAR,
      ONCOTREE_CODE        VARCHAR,
      SAMPLE_TYPE          VARCHAR,
      SEQ_ASSAY_ID         VARCHAR,
      CANCER_TYPE          VARCHAR,
      CANCER_TYPE_DETAILED VARCHAR,
      SAMPLE_TYPE_DETAILED VARCHAR,
      CENTER               VARCHAR,
      SEX                  VARCHAR,
      PRIMARY_RACE         VARCHAR,
      ETHNICITY            VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS mutations (
      release_version         VARCHAR,
      Hugo_Symbol             VARCHAR,
      Entrez_Gene_Id          VARCHAR,
      Center                  VARCHAR,
      NCBI_Build              VARCHAR,
      Chromosome              VARCHAR,
      Start_Position          BIGINT,
      End_Position            BIGINT,
      Strand                  VARCHAR,
      Consequence             VARCHAR,
      Variant_Classification  VARCHAR,
      Variant_Type            VARCHAR,
      Reference_Allele        VARCHAR,
      Tumor_Seq_Allele1       VARCHAR,
      Tumor_Seq_Allele2       VARCHAR,
      dbSNP_RS                VARCHAR,
      Tumor_Sample_Barcode    VARCHAR,
      HGVSc                   VARCHAR,
      HGVSp                   VARCHAR,
      HGVSp_Short             VARCHAR,
      Transcript_ID           VARCHAR,
      RefSeq                  VARCHAR,
      Exon_Number             VARCHAR,
      t_depth                 DOUBLE,
      t_ref_count             DOUBLE,
      t_alt_count             DOUBLE,
      n_depth                 DOUBLE,
      n_ref_count             DOUBLE,
      n_alt_count             DOUBLE,
      FILTER                  VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS cna (
      release_version      VARCHAR,
      Hugo_Symbol          VARCHAR,
      Tumor_Sample_Barcode VARCHAR,
      CNA_value            INTEGER
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS sv (
      release_version        VARCHAR,
      Center                 VARCHAR,
      Sample_Id              VARCHAR,
      SV_Status              VARCHAR,
      Site1_Hugo_Symbol      VARCHAR,
      Site2_Hugo_Symbol      VARCHAR,
      Site1_Chromosome       VARCHAR,
      Site2_Chromosome       VARCHAR,
      Site1_Position         BIGINT,
      Site2_Position         BIGINT,
      Class                  VARCHAR,
      Event_Info             VARCHAR,
      DNA_Support            VARCHAR,
      RNA_Support            VARCHAR,
      Annotation             VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS gene_matrix (
      release_version VARCHAR,
      SAMPLE_ID       VARCHAR,
      mutations       VARCHAR,
      cna             VARCHAR,
      fusions         VARCHAR,
      structural_variants VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS assay_information (
      release_version    VARCHAR,
      SEQ_ASSAY_ID       VARCHAR,
      is_paired_end      VARCHAR,
      library_selection  VARCHAR,
      library_strategy   VARCHAR,
      platform           VARCHAR,
      read_length        DOUBLE,
      target_capture_kit VARCHAR,
      instrument_model   VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS genomic_information (
      release_version VARCHAR,
      Chromosome      VARCHAR,
      Start_Position  BIGINT,
      End_Position    BIGINT,
      Hugo_Symbol     VARCHAR,
      SEQ_ASSAY_ID    VARCHAR,
      Feature_Type    VARCHAR,
      includeVariants VARCHAR
    )
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS bed (
      release_version VARCHAR,
      Chromosome      VARCHAR,
      Start_Position  BIGINT,
      End_Position    BIGINT,
      Hugo_Symbol     VARCHAR,
      ID              VARCHAR,
      SEQ_ASSAY_ID    VARCHAR
    )
  "
  )

  message("Database ready: ", db_path)
  con
}
