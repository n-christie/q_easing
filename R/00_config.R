# =============================================================================
# 00_config.R — Global Configuration
# Sourced by all scripts. Sets study parameters, paths, logging, and helpers.
# =============================================================================

suppressPackageStartupMessages({
  library(logger)
  library(fs)
  library(tidyverse)
  library(glue)
})

# -----------------------------------------------------------------------------
# Study parameters
# -----------------------------------------------------------------------------

STUDY_START <- "2005-01-01"
STUDY_END   <- as.character(Sys.Date())

COUNTRIES <- c("US", "EA", "NO", "SE", "DK", "JP", "GB", "CH")

BIS_COUNTRY_CODES <- c(
  US = "US",
  EA = "XM",
  NO = "NO",
  SE = "SE",
  DK = "DK",
  JP = "JP",
  GB = "GB",
  CH = "CH"
)

WB_COUNTRY_CODES <- c(
  US = "US",
  EA = "EMU",
  NO = "NOR",
  SE = "SWE",
  DK = "DNK",
  JP = "JPN",
  GB = "GBR",
  CH = "CHE"
)

# -----------------------------------------------------------------------------
# FRED API key
# -----------------------------------------------------------------------------

FRED_API_KEY <- Sys.getenv("FRED_API_KEY")
if (nchar(FRED_API_KEY) == 0) {
  warning(
    "FRED_API_KEY environment variable is not set. ",
    "FRED-dependent data collection will fail. ",
    "Set it in ~/.Renviron as: FRED_API_KEY=your_key_here"
  )
}

# -----------------------------------------------------------------------------
# Directory paths
# -----------------------------------------------------------------------------

PROJECT_ROOT  <- here::here()   # falls back to getwd() if here not available
DIR_DATA      <- file.path(PROJECT_ROOT, "data")
DIR_RAW       <- file.path(DIR_DATA, "raw")
DIR_PROCESSED <- file.path(DIR_DATA, "processed")
DIR_MANUAL    <- file.path(DIR_DATA, "manual")
DIR_OUTPUT    <- file.path(PROJECT_ROOT, "output")
DIR_FIGURES   <- file.path(DIR_OUTPUT, "figures")
DIR_LOGS      <- file.path(DIR_OUTPUT, "logs")

# Raw subdirectories
DIR_RAW_FRED      <- file.path(DIR_RAW, "fred")
DIR_RAW_BIS       <- file.path(DIR_RAW, "bis")
DIR_RAW_ECB       <- file.path(DIR_RAW, "ecb")
DIR_RAW_WORLDBANK <- file.path(DIR_RAW, "worldbank")
DIR_RAW_SHILLER   <- file.path(DIR_RAW, "shiller")
DIR_RAW_CHINNITO  <- file.path(DIR_RAW, "chinnito")

# Create all directories (idempotent)
fs::dir_create(c(
  DIR_RAW_FRED, DIR_RAW_BIS, DIR_RAW_ECB,
  DIR_RAW_WORLDBANK, DIR_RAW_SHILLER, DIR_RAW_CHINNITO,
  DIR_PROCESSED, DIR_MANUAL, DIR_FIGURES, DIR_LOGS
))

# -----------------------------------------------------------------------------
# Logging — tee to timestamped file and console
# -----------------------------------------------------------------------------

log_file <- file.path(
  DIR_LOGS,
  glue("pipeline_{format(Sys.time(), '%Y%m%d_%H%M%S')}.log")
)

log_appender(appender_tee(log_file))
log_threshold(DEBUG)
log_formatter(formatter_glue)

log_info("Configuration loaded. Study period: {STUDY_START} to {STUDY_END}")
log_info("Log file: {log_file}")

# -----------------------------------------------------------------------------
# Helper: safe_fetch
# Wraps an expression in tryCatch; returns NULL on error and logs the failure.
# Usage: safe_fetch(some_api_call(), source_name="BIS", series_id="US CBTA")
# -----------------------------------------------------------------------------

safe_fetch <- function(expr, source_name = "unknown", series_id = "unknown") {
  tryCatch(
    {
      result <- expr
      log_info("Fetched OK  | source={source_name} | series={series_id}")
      result
    },
    error = function(e) {
      log_error(
        "Fetch FAILED | source={source_name} | series={series_id} | error={conditionMessage(e)}"
      )
      NULL
    },
    warning = function(w) {
      log_warn(
        "Fetch WARN  | source={source_name} | series={series_id} | warning={conditionMessage(w)}"
      )
      # Re-evaluate to get the result despite the warning
      withCallingHandlers(
        expr,
        warning = function(w) invokeRestart("muffleWarning")
      )
    }
  )
}

# -----------------------------------------------------------------------------
# Helper: save_raw
# Saves a data object as .rds to data/raw/{source}/
# -----------------------------------------------------------------------------

save_raw <- function(data, source, filename_stem) {
  dir <- file.path(DIR_RAW, source)
  fs::dir_create(dir)
  path <- file.path(dir, paste0(filename_stem, ".rds"))
  saveRDS(data, path)
  log_info("Saved raw   | {path}")
  invisible(path)
}

# -----------------------------------------------------------------------------
# Helper: save_processed
# Saves a data frame as both .rds and .csv to data/processed/
# -----------------------------------------------------------------------------

save_processed <- function(data, filename_stem) {
  rds_path <- file.path(DIR_PROCESSED, paste0(filename_stem, ".rds"))
  csv_path <- file.path(DIR_PROCESSED, paste0(filename_stem, ".csv"))
  saveRDS(data, rds_path)
  write_csv(data, csv_path)
  log_info("Saved processed | {rds_path}")
  log_info("Saved processed | {csv_path}")
  invisible(list(rds = rds_path, csv = csv_path))
}

# -----------------------------------------------------------------------------
# Availability tracking — updated by each script, consolidated in 05
# -----------------------------------------------------------------------------

AVAILABILITY <- list()
