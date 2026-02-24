# =============================================================================
# 06_collect_manual.R — Programmatic download of "manual" data files
# Targets:
#   data/manual/cb_assets_NO.csv  — Norges Bank total assets (SSB table 08428)
#                                   Units: NOK million, monthly from 2009M05
#   data/manual/cb_assets_SE.csv  — Riksbank total assets (Excel time-series)
#                                   Units: SEK million, weekly -> monthly avg
#   data/manual/cb_assets_DK.csv  — Danmarks Nationalbank (DST table DNSNB1)
#                                   Units: DKK billion -> converted to million
#   data/manual/equity_valuation.csv — Damodaran annual country P/E
#                                   (annual, expanded to monthly by carry-forward)
# =============================================================================

source(here::here("R/00_config.R"))
suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(readxl)
  library(jsonlite)
})

MANUAL_DIR <- file.path(here::here(), "data/manual")
fs::dir_create(MANUAL_DIR)

# =============================================================================
# 1. Norway — SSB PxWebApi v0, table 08428
#    Variable codes: BalanseFinans=10 (total assets), ContentsCode=Balanse100
#    Units: NOK million, monthly from 2009M05
# =============================================================================

log_info("=== 1. Norway CB assets (SSB table 08428) ===")

ssb_url  <- "https://data.ssb.no/api/v0/en/table/08428"
ssb_body <- list(
  query = list(
    list(code = "BalanseFinans",
         selection = list(filter = "item", values = list("10"))),
    list(code = "ContentsCode",
         selection = list(filter = "item", values = list("Balanse100"))),
    list(code = "Tid",
         selection = list(filter = "all", values = list("*")))
  ),
  response = list(format = "csv2")
)

ssb_resp <- tryCatch(
  httr::POST(
    ssb_url,
    httr::content_type("application/json"),
    body    = jsonlite::toJSON(ssb_body, auto_unbox = TRUE),
    httr::timeout(60)
  ),
  error = function(e) { log_error("SSB request failed: {e$message}"); NULL }
)

if (!is.null(ssb_resp) && httr::status_code(ssb_resp) == 200) {
  ssb_text <- httr::content(ssb_resp, as = "text", encoding = "UTF-8")
  # csv2 format: "balance sheet","month","contents","08428: ..."
  ssb_raw  <- read.csv(text = ssb_text, stringsAsFactors = FALSE, quote = '"')
  log_info("SSB raw: {nrow(ssb_raw)} rows, cols: {paste(names(ssb_raw), collapse=', ')}")

  # The value column is the last one (contains the 08428: header name)
  value_col <- names(ssb_raw)[ncol(ssb_raw)]
  # Month column: e.g. "2009M05"
  month_col <- grep("(?i)month|tid|time|period", names(ssb_raw), value = TRUE)[1]

  cb_no <- ssb_raw |>
    rename(period = all_of(month_col), cb_assets = all_of(value_col)) |>
    mutate(
      year   = as.integer(substr(period, 1, 4)),
      month  = as.integer(sub(".*M", "", period)),
      date   = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
      cb_assets = suppressWarnings(as.numeric(cb_assets))
    ) |>
    filter(!is.na(date), !is.na(cb_assets), is.finite(cb_assets)) |>
    select(date, cb_assets) |>
    arrange(date)

  out_no <- file.path(MANUAL_DIR, "cb_assets_NO.csv")
  write.csv(cb_no, out_no, row.names = FALSE)
  log_info("Norway saved: {nrow(cb_no)} rows, {min(cb_no$date)} to {max(cb_no$date)}")
  log_info("  Units: NOK million (end-of-month stock)")
  log_info("  NOTE: SSB data starts 2009-05. Pre-2009 gap filled by BIS CBTA in panel.")
} else {
  status <- if (!is.null(ssb_resp)) httr::status_code(ssb_resp) else "NULL"
  log_error("SSB API failed (status {status}) — Norway not saved")
}

# =============================================================================
# 2. Sweden — Riksbank time-series Excel
#    Col 1 = date (Excel serial), Col 31 = "Total assets", SEK million
#    Weekly data -> average to monthly
# =============================================================================

log_info("=== 2. Sweden CB assets (Riksbank time-series Excel) ===")

riksbank_page <- "https://www.riksbank.se/en-gb/statistics/riksbanks-balance-sheet/the-riksbanks-assets-and-liabilities-the-weekly-report/"
xl_dest       <- file.path(DIR_RAW, "riksbank", "riksbank_balance_sheet.xlsx")
fs::dir_create(file.path(DIR_RAW, "riksbank"))

# Scrape page for Excel link (re-download if not fresh)
riks_page_resp <- tryCatch(
  httr::GET(riksbank_page, httr::timeout(30)),
  error = function(e) { log_error("Riksbank page fetch failed: {e$message}"); NULL }
)

if (!is.null(riks_page_resp) && httr::status_code(riks_page_resp) == 200) {
  page_html <- httr::content(riks_page_resp, as = "text", encoding = "UTF-8")
  links     <- rvest::read_html(page_html) |>
    rvest::html_nodes("a") |> rvest::html_attr("href")
  xl_links  <- grep("\\.(xlsx|xls)$", links, value = TRUE, ignore.case = TRUE)

  if (length(xl_links) > 0) {
    xl_url <- xl_links[1]
    if (!grepl("^https?://", xl_url)) xl_url <- paste0("https://www.riksbank.se", xl_url)
    log_info("Riksbank Excel URL: {xl_url}")

    dl_resp <- tryCatch(
      httr::GET(xl_url, httr::write_disk(xl_dest, overwrite = TRUE), httr::timeout(120)),
      error = function(e) { log_error("Riksbank Excel download failed: {e$message}"); NULL }
    )
    if (is.null(dl_resp) || httr::status_code(dl_resp) != 200) {
      log_error("Riksbank Excel download failed — using cached file if present")
    } else {
      log_info("Riksbank Excel downloaded ({file.size(xl_dest)} bytes)")
    }
  }
}

# Parse the Riksbank Excel
# Structure (confirmed by inspection):
#   Row 1: title
#   Row 2: column headers (col 1 = "Dates, Weekly Report"; col 31 = "Total assets")
#   Row 3: (sub-header / blank)
#   Rows 4+: data (col 1 = Excel date serial, col 31 = SEK million total assets)

if (file.exists(xl_dest)) {
  riks_raw <- tryCatch(
    readxl::read_excel(xl_dest, sheet = 1, col_names = FALSE),
    error = function(e) { log_error("Riksbank Excel read failed: {e$message}"); NULL }
  )

  if (!is.null(riks_raw)) {
    # Col 31 = total assets (1-indexed in readxl = col index 31)
    date_col   <- 1
    assets_col <- 31

    # Verify header
    header_label <- as.character(riks_raw[2, assets_col])
    log_info("Riksbank col 31 header: '{header_label}'")

    # Data starts at row 4 (rows 1-3 are headers)
    riks_data <- riks_raw[4:nrow(riks_raw), c(date_col, assets_col)]
    names(riks_data) <- c("date_serial", "cb_assets")

    cb_se <- riks_data |>
      mutate(
        date_serial = suppressWarnings(as.numeric(date_serial)),
        cb_assets   = suppressWarnings(as.numeric(cb_assets)),
        # Excel serial -> R date
        date        = as.Date(date_serial, origin = "1899-12-30"),
        year        = as.integer(format(date, "%Y")),
        month       = as.integer(format(date, "%m"))
      ) |>
      filter(
        !is.na(date), !is.na(cb_assets), is.finite(cb_assets),
        year >= 2004
      ) |>
      # Average weekly observations to monthly
      group_by(year, month) |>
      summarise(cb_assets = mean(cb_assets, na.rm = TRUE), .groups = "drop") |>
      mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) |>
      select(date, cb_assets) |>
      arrange(date)

    out_se <- file.path(MANUAL_DIR, "cb_assets_SE.csv")
    write.csv(cb_se, out_se, row.names = FALSE)
    log_info("Sweden saved: {nrow(cb_se)} rows, {min(cb_se$date)} to {max(cb_se$date)}")
    log_info("  Units: SEK million (monthly average of weekly totals)")
  }
} else {
  log_warn("Riksbank Excel not found — Sweden CB assets not saved")
}

# =============================================================================
# 3. Denmark — Statistics Denmark API, table DNSNB1
#    SPECNAT=I (not applicable/total), INSTRUMENT=ATA (total assets)
#    Units: DKK billion -> multiply by 1000 for millions
# =============================================================================

log_info("=== 3. Denmark CB assets (DST table DNSNB1) ===")

dst_url  <- "https://api.statbank.dk/v1/data"
dst_body <- list(
  table     = "DNSNB1",
  format    = "CSV",
  lang      = "en",
  variables = list(
    list(code = "SPECNAT",    values = list("I")),   # not applicable (overall total)
    list(code = "INSTRUMENT", values = list("ATA")), # Total assets
    list(code = "Tid",        values = list("*"))    # all periods
  )
)

dst_resp <- tryCatch(
  httr::POST(
    dst_url,
    httr::content_type("application/json"),
    body    = jsonlite::toJSON(dst_body, auto_unbox = TRUE),
    httr::timeout(60)
  ),
  error = function(e) { log_error("DST request failed: {e$message}"); NULL }
)

if (!is.null(dst_resp) && httr::status_code(dst_resp) == 200) {
  dst_text <- httr::content(dst_resp, as = "text", encoding = "UTF-8")
  # Remove BOM if present
  dst_text <- sub("^\xef\xbb\xbf", "", dst_text)
  dst_raw  <- read.csv(text = dst_text, stringsAsFactors = FALSE, sep = ";",
                       encoding = "UTF-8")
  log_info("DST raw: {nrow(dst_raw)} rows, cols: {paste(names(dst_raw), collapse=', ')}")

  # Columns: SPECNAT, INSTRUMENT, TID, INDHOLD
  # TID format: "2005M01", INDHOLD = value (DKK billion)
  cb_dk <- dst_raw |>
    rename(period = TID, cb_assets_bn = INDHOLD) |>
    mutate(
      year       = as.integer(substr(period, 1, 4)),
      month      = as.integer(sub(".*M", "", period)),
      date       = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
      cb_assets_bn = suppressWarnings(as.numeric(gsub(",", ".", cb_assets_bn))),
      cb_assets  = cb_assets_bn * 1000   # DKK billion -> million
    ) |>
    filter(!is.na(date), !is.na(cb_assets), is.finite(cb_assets),
           year >= 2004) |>
    select(date, cb_assets) |>
    arrange(date)

  out_dk <- file.path(MANUAL_DIR, "cb_assets_DK.csv")
  write.csv(cb_dk, out_dk, row.names = FALSE)
  log_info("Denmark saved: {nrow(cb_dk)} rows, {min(cb_dk$date)} to {max(cb_dk$date)}")
  log_info("  Units: DKK million (converted from DKK billion)")
} else {
  status <- if (!is.null(dst_resp)) httr::status_code(dst_resp) else "NULL"
  log_error("DST API failed (status {status}) — Denmark not saved")
}

# =============================================================================
# 4. Equity Valuation — Damodaran annual country P/E
#    Source: NYU Stern (Aswath Damodaran), updated annually in January
#    Covers: broad set of countries including NO, SE, DK, JP, GB, CH, EA proxy
#    Note: This is TRAILING P/E, not CAPE. Annual frequency only.
# =============================================================================

log_info("=== 4. Equity valuation — Damodaran annual country P/E ===")

damodaran_url  <- "https://pages.stern.nyu.edu/~adamodar/pc/datasets/countrystats.xls"
damodaran_dest <- file.path(DIR_RAW, "damodaran", "countrystats.xls")
fs::dir_create(file.path(DIR_RAW, "damodaran"))

damod_resp <- tryCatch(
  httr::GET(
    damodaran_url,
    httr::write_disk(damodaran_dest, overwrite = TRUE),
    httr::timeout(120),
    httr::user_agent("Mozilla/5.0 (academic research)")
  ),
  error = function(e) { log_error("Damodaran download failed: {e$message}"); NULL }
)

if (!is.null(damod_resp) && httr::status_code(damod_resp) == 200) {
  log_info("Damodaran file downloaded: {damodaran_dest} ({file.size(damodaran_dest)} bytes)")
  sheets <- tryCatch(readxl::excel_sheets(damodaran_dest), error = function(e) character(0))
  log_info("Sheets: {paste(sheets, collapse=', ')}")

  # Damodaran's countrystats.xls: 8 metadata rows, then header row, then data
  damod_raw <- tryCatch(
    readxl::read_excel(damodaran_dest, sheet = 1, skip = 8),
    error = function(e) { log_error("Damodaran parse error: {e$message}"); NULL }
  )

  if (!is.null(damod_raw)) {
    log_info("Damodaran raw: {nrow(damod_raw)} rows x {ncol(damod_raw)} cols")

    country_col <- "Country"
    # Use trailing P/E as most comparable to CAPE concept
    pe_col      <- grep("(?i)trailing.*pe|median.*trailing", names(damod_raw),
                        value = TRUE, perl = TRUE)[1]
    if (is.na(pe_col)) pe_col <- grep("(?i)current.*pe|median.*current",
                                       names(damod_raw), value = TRUE, perl = TRUE)[1]
    fwd_pe_col  <- grep("(?i)forward.*pe|median.*forward", names(damod_raw),
                        value = TRUE, perl = TRUE)[1]
    log_info("Country col: '{country_col}', Trailing P/E col: '{pe_col}', Fwd P/E: '{fwd_pe_col}'")

    # Country name -> our 2-letter codes
    # Damodaran uses full country names; map to our codes
    damod_country_map <- c(
      "Norway"             = "NO",
      "Sweden"             = "SE",
      "Denmark"            = "DK",
      "Japan"              = "JP",
      "United Kingdom"     = "GB",
      "Switzerland"        = "CH",
      "Germany"            = "EA",   # EA proxy
      "Eurozone"           = "EA",
      "Euro area"          = "EA",
      "United States"      = "US"
    )

    if (!is.na(pe_col)) {
      damod_mapped <- damod_raw |>
        rename(country_name = all_of(country_col), pe_trailing = all_of(pe_col)) |>
        mutate(
          country_name = trimws(as.character(country_name)),
          pe_trailing  = suppressWarnings(as.numeric(pe_trailing)),
          pe_forward   = if (!is.na(fwd_pe_col))
                           suppressWarnings(as.numeric(.data[[fwd_pe_col]]))
                         else NA_real_,
          country      = damod_country_map[country_name]
        ) |>
        filter(!is.na(country), !is.na(pe_trailing), is.finite(pe_trailing)) |>
        select(country, pe_trailing, pe_forward)

      log_info("Damodaran matched countries: {paste(sort(unique(damod_mapped$country)), collapse=', ')}")
      log_info("Sample P/E values:")
      print(damod_mapped)

      # Damodaran is CURRENT year's data (published January of each year)
      # Add as the current year's snapshot; year = year of download
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      # Expand to monthly for current year
      damod_long <- damod_mapped |>
        mutate(year = current_year) |>
        tidyr::crossing(month = 1:12) |>
        mutate(
          date   = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
          cape   = NA_real_
        ) |>
        select(date, country, cape, pe_trailing, pe_forward) |>
        arrange(country, date)

      out_eq <- file.path(MANUAL_DIR, "equity_valuation.csv")
      write.csv(damod_long, out_eq, row.names = FALSE)
      log_info("Equity valuation saved: {nrow(damod_long)} rows")
      log_info("  Countries: {paste(sort(unique(damod_long$country)), collapse=', ')}")
      log_info("  NOTE: Damodaran data is current-year trailing P/E only (annual snapshot).")
      log_info("  CAPE (Shiller) for non-US countries must be added manually for main analysis.")
    } else {
      log_warn("P/E column not found in Damodaran file. Inspect manually: {damodaran_dest}")
    }
  }
} else {
  status <- if (!is.null(damod_resp)) httr::status_code(damod_resp) else "NULL"
  log_error("Damodaran download failed (status {status})")
  log_warn("Equity valuation file not saved — manual entry required.")
  log_warn("  See data/manual/README.md for instructions.")
}

# =============================================================================
# Summary
# =============================================================================

log_info("=== Summary ===")
files_check <- c("cb_assets_NO.csv", "cb_assets_SE.csv", "cb_assets_DK.csv",
                 "equity_valuation.csv")
for (f in files_check) {
  path   <- file.path(MANUAL_DIR, f)
  status <- if (file.exists(path)) {
    nrow_val <- tryCatch(nrow(read.csv(path)), error = function(e) "?")
    paste0("PRESENT (", nrow_val, " rows)")
  } else "MISSING"
  log_info("  {f}: {status}")
}

log_info("Script 06 complete.")
log_info("Next: rerun R/04_clean_merge.R and R/05_descriptives.R to incorporate new files.")
