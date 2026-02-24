# =============================================================================
# 02_collect_equity.R — CAPE and P/E Data
# Sources: Shiller (US CAPE), OECD PBR (non-US partial substitute)
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(readxl)
  library(lubridate)
})

# =============================================================================
# Section 1: Shiller US CAPE (primary source for US equity valuation)
# =============================================================================
# Data from: http://www.econ.yale.edu/~shiller/data/ie_data.xls
# Sheet "Data", skip 7 rows, date column is "YYYY.MM" format.
# CAPE = cyclically adjusted P/E (10-year real earnings avg).
# =============================================================================

log_info("=== Section 1: Shiller US CAPE ===")

shiller_destfile <- file.path(DIR_RAW_SHILLER, "ie_data.xls")

shiller_download_ok <- safe_fetch(
  download.file(
    url     = "http://www.econ.yale.edu/~shiller/data/ie_data.xls",
    destfile = shiller_destfile,
    mode    = "wb",
    quiet   = TRUE
  ),
  source_name = "Shiller",
  series_id   = "ie_data.xls"
)

if (!is.null(shiller_download_ok)) {
  log_info("Shiller file downloaded to {shiller_destfile}")

  # Read with explicit col_names to avoid header confusion.
  # Row 8 (after skipping 7) is the actual header row in the XLS.
  # Columns of interest: Date (col 1), P (col 2), D (col 3), E (col 4),
  #   CPI (col 5), GS10 (col 7), CAPE (col 15 in recent versions).
  # We read all columns then select by name pattern.

  shiller_raw <- safe_fetch(
    readxl::read_excel(
      shiller_destfile,
      sheet = "Data",
      skip  = 7
    ),
    source_name = "Shiller",
    series_id   = "ie_data sheet"
  )

  if (!is.null(shiller_raw)) {
    save_raw(shiller_raw, "shiller", "ie_data_raw")
    log_info("Shiller raw: {nrow(shiller_raw)} rows, columns: {paste(names(shiller_raw)[1:min(10,ncol(shiller_raw))], collapse=', ')}")

    # The first column is the date in "YYYY.MM" decimal format.
    # Find it by position (first column) or by name containing "Date".
    date_col_idx <- which(grepl("date|Date|DATE", names(shiller_raw)))[1]
    if (is.na(date_col_idx)) date_col_idx <- 1

    # Identify P (price), E (earnings, monthly), CAPE columns
    # Column names in Shiller data vary across versions; use flexible matching.
    col_names_lower <- tolower(names(shiller_raw))

    price_col <- which(col_names_lower == "p")[1]
    if (is.na(price_col)) price_col <- which(grepl("^p$|price", col_names_lower))[1]

    earn_col  <- which(col_names_lower == "e")[1]
    if (is.na(earn_col)) earn_col <- which(grepl("^e$|earn", col_names_lower))[1]

    # CAPE is typically labeled "CAPE" or "P/E10" or "Cyclically"
    cape_col  <- which(grepl("cape|p/e10|pe10|cyclically", col_names_lower))[1]

    log_info("Shiller column indices — date:{date_col_idx}, P:{price_col}, E:{earn_col}, CAPE:{cape_col}")

    shiller_clean <- shiller_raw |>
      rename(date_raw = all_of(date_col_idx)) |>
      filter(!is.na(date_raw)) |>
      mutate(
        # date_raw is like 2005.01 (numeric) or "2005.01" (character)
        date_str = as.character(date_raw),
        # Some rows have "2005.1" not "2005.01" — pad the decimal part
        date_str = case_when(
          str_detect(date_str, "\\.") ~ {
            parts <- str_split_fixed(date_str, "\\.", 2)
            str_c(parts[, 1], ".", str_pad(parts[, 2], 2, "right", "0"))
          },
          TRUE ~ date_str
        ),
        year  = as.integer(str_sub(date_str, 1, 4)),
        month = as.integer(str_sub(date_str, 6, 7)),
        date  = make_date(year, month, 1)
      ) |>
      filter(!is.na(date), year >= 2005)

    # Add price, earnings, CAPE
    if (!is.na(price_col)) {
      shiller_clean <- shiller_clean |>
        mutate(price = suppressWarnings(as.numeric(.data[[names(shiller_raw)[price_col]]])))
    } else {
      shiller_clean <- shiller_clean |> mutate(price = NA_real_)
      log_warn("Shiller: could not identify price column")
    }

    if (!is.na(earn_col)) {
      shiller_clean <- shiller_clean |>
        mutate(
          earnings_annual = suppressWarnings(as.numeric(.data[[names(shiller_raw)[earn_col]]])),
          # P/E = price / (earnings_annual / 12) but Shiller E is annual
          # Simple trailing P/E = P / (E/12 * 12) = P / E
          pe_ratio = price / earnings_annual
        )
    } else {
      shiller_clean <- shiller_clean |> mutate(earnings_annual = NA_real_, pe_ratio = NA_real_)
      log_warn("Shiller: could not identify earnings column")
    }

    if (!is.na(cape_col)) {
      shiller_clean <- shiller_clean |>
        mutate(cape = suppressWarnings(as.numeric(.data[[names(shiller_raw)[cape_col]]])))
    } else {
      shiller_clean <- shiller_clean |> mutate(cape = NA_real_)
      log_warn("Shiller: could not identify CAPE column — may need column index update")
    }

    shiller_output <- shiller_clean |>
      transmute(
        date,
        country  = "US",
        cape,
        pe_ratio,
        source   = "Shiller"
      ) |>
      filter(!is.na(cape) | !is.na(pe_ratio)) |>
      arrange(date)

    save_processed(shiller_output, "shiller_cape_us")
    log_info("Shiller CAPE US: {nrow(shiller_output)} rows ({min(shiller_output$date)} to {max(shiller_output$date)})")

    AVAILABILITY[["shiller_cape_us"]] <- list(
      status     = "ok",
      rows       = nrow(shiller_output),
      date_range = c(min(shiller_output$date), max(shiller_output$date)),
      cape_ok    = !all(is.na(shiller_output$cape)),
      pe_ok      = !all(is.na(shiller_output$pe_ratio))
    )
  } else {
    log_error("Shiller: failed to parse Excel file")
    AVAILABILITY[["shiller_cape_us"]] <- list(status = "parse_failed")
  }
} else {
  log_error("Shiller: download failed")
  AVAILABILITY[["shiller_cape_us"]] <- list(status = "download_failed")
}

# =============================================================================
# Section 2: Non-US Equity Valuation — OECD Price-to-Book (partial substitute)
# =============================================================================
# NOTE: This is PBR (price-to-book ratio), NOT CAPE. It is NOT a substitute
# for the regression's main dependent variable. It is collected as supplementary
# context only. Non-US CAPE for regressions must come from data/manual/.
# =============================================================================

log_info("=== Section 2: OECD Price-to-Book (supplementary, NOT a CAPE substitute) ===")
log_warn("OECD PBR is price-to-BOOK, not CAPE. Do NOT use as main DV in regressions.")

oecd_pbr_url <- paste0(
  "https://stats.oecd.org/SDMX-JSON/data/FIN_IND_FBS/",
  "USA+EMU+NOR+SWE+DNK+JPN+GBR+CHE.PBR/all",
  "?startPeriod=2005&format=csv"
)

oecd_pbr_raw <- safe_fetch(
  {
    resp <- httr::GET(oecd_pbr_url, httr::timeout(60))
    if (httr::http_error(resp)) stop(paste("HTTP", httr::status_code(resp)))
    httr::content(resp, "text", encoding = "UTF-8")
  },
  source_name = "OECD",
  series_id   = "FIN_IND_FBS PBR"
)

if (!is.null(oecd_pbr_raw)) {
  save_raw(oecd_pbr_raw, "bis", "oecd_pbr_raw")   # stored in bis for convenience

  oecd_pbr_parsed <- tryCatch(
    read_csv(I(oecd_pbr_raw), show_col_types = FALSE),
    error = function(e) {
      log_error("OECD PBR parse failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (!is.null(oecd_pbr_parsed) && nrow(oecd_pbr_parsed) > 0) {
    log_info("OECD PBR raw columns: {paste(names(oecd_pbr_parsed), collapse=', ')}")
    save_raw(oecd_pbr_parsed, "bis", "oecd_pbr_parsed")

    AVAILABILITY[["oecd_pbr"]] <- list(
      status = "ok",
      rows   = nrow(oecd_pbr_parsed),
      note   = "PBR (price-to-book), NOT CAPE — supplementary only"
    )
  } else {
    AVAILABILITY[["oecd_pbr"]] <- list(status = "parse_failed", note = "PBR only, not CAPE")
  }
} else {
  log_warn("OECD PBR fetch failed — non-US equity valuation must be entered manually")
  AVAILABILITY[["oecd_pbr"]] <- list(
    status = "fetch_failed",
    note   = "Non-US CAPE must come from data/manual/equity_valuation.csv"
  )
}

log_warn(paste(
  "NON-US CAPE: Not available via free API.",
  "For cross-country regressions, populate data/manual/equity_valuation.csv",
  "from Star Capital (starcapital.de) or MSCI monthly factsheets.",
  "See data/manual/README.md for column spec."
))

# =============================================================================
# Save availability record for this script
# =============================================================================

saveRDS(AVAILABILITY, file.path(DIR_PROCESSED, "availability_02_equity.rds"))
log_info("Script 02 complete. Availability saved.")
