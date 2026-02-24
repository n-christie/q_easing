# =============================================================================
# 03_collect_controls.R — ECB Macro Data, Money Supply, Chinn-Ito
# Sources: ECB via `ecb` package, FRED (M2), Chinn-Ito website
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(ecb)
  library(fredr)
  library(rvest)
  library(readxl)
  library(lubridate)
  library(httr)
})

fredr_set_key(FRED_API_KEY)

# =============================================================================
# Section 1: ECB Data via `ecb` package
# =============================================================================
# ecb::get_data(key) returns a data frame with `obstime` (YYYY-MM) and
# `obsvalue` (numeric). Filter to study period.
# Note: ECB ICP dataset is being replaced as of Feb 2026 — key may fail.
# =============================================================================

log_info("=== Section 1: ECB Macro Data ===")

ecb_series_specs <- list(
  ea_cpi       = list(
    key      = "ICP.M.U2.N.000000.4.ANR",
    variable = "cpi_yoy",
    note     = "EA HICP, annual rate of change. Key may be deprecated post-Feb 2026."
  ),
  ea_yield_10y = list(
    key      = "FM.M.U2.EUR.4F.BB.U2_10Y.YLD",
    variable = "yield_10y",
    note     = "EA 10-year government bond yield"
  ),
  ea_m3        = list(
    key      = "BSI.M.U2.N.V.M30.X.1.U2.2300.Z01.E",
    variable = "m3",
    note     = "EA M3 money supply"
  ),
  ea_cb_assets = list(
    key      = "BSI.M.U2.N.A.A20T.A.1.Z5.0000.Z01.E",
    variable = "cb_assets_ecb",
    note     = "ECB total assets (consolidated Eurosystem balance sheet)"
  )
)

# Fallback HICP key in case ICP is deprecated
ecb_hicp_fallback_key <- "ICP.M.U2.N.000000.4.INX"  # index form, may need conversion

fetch_ecb_series <- function(spec, name) {
  ekey  <- spec$key
  enote <- spec$note
  log_info("ECB: {name} - key={ekey}")
  if (!is.null(enote)) log_info("  Note: {enote}")

  raw <- safe_fetch(
    ecb::get_data(ekey),
    source_name = "ECB",
    series_id   = ekey
  )

  # If primary key fails for CPI, try fallback
  if (is.null(raw) && name == "ea_cpi") {
    log_warn("ECB CPI primary key failed. Trying fallback: {ecb_hicp_fallback_key}")
    raw <- safe_fetch(
      ecb::get_data(ecb_hicp_fallback_key),
      source_name = "ECB_fallback",
      series_id   = ecb_hicp_fallback_key
    )
    if (!is.null(raw)) {
      log_info("ECB CPI fallback key succeeded — note: this is index form, not annual rate")
    }
  }

  if (is.null(raw)) return(NULL)
  if (nrow(raw) == 0) {
    log_warn("ECB {name}: returned 0 rows")
    return(NULL)
  }

  save_raw(raw, "ecb", name)

  # Standardise: obstime -> date, obsvalue -> variable value
  if (!"obstime" %in% names(raw) || !"obsvalue" %in% names(raw)) {
    log_error("ECB {name}: unexpected column names: {paste(names(raw), collapse=', ')}")
    return(NULL)
  }

  evar <- spec$variable
  cleaned <- raw |>
    transmute(
      date     = lubridate::ym(obstime),
      country  = "EA",
      variable = evar,
      value    = suppressWarnings(as.numeric(obsvalue))
    ) |>
    filter(
      !is.na(date),
      !is.na(value),
      date >= as.Date(STUDY_START),
      date <= as.Date(STUDY_END)
    )

  log_info("ECB {name}: {nrow(cleaned)} rows ({min(cleaned$date)} to {max(cleaned$date)})")
  cleaned
}

ecb_results <- imap(ecb_series_specs, fetch_ecb_series)
ecb_ok      <- keep(ecb_results, ~ !is.null(.x))

AVAILABILITY[["ecb_macro"]] <- list(
  attempted = names(ecb_series_specs),
  succeeded = names(ecb_ok),
  failed    = setdiff(names(ecb_series_specs), names(ecb_ok))
)

if (length(ecb_ok) > 0) {
  ecb_long <- bind_rows(ecb_ok)

  ecb_wide <- ecb_long |>
    pivot_wider(names_from = variable, values_from = value) |>
    arrange(date)

  save_processed(ecb_wide, "ecb_macro_monthly")
  log_info("ECB macro wide: {nrow(ecb_wide)} rows, columns: {paste(names(ecb_wide), collapse=', ')}")
} else {
  log_warn("No ECB series collected")
}

# =============================================================================
# Section 2: Money Supply via FRED
# =============================================================================

log_info("=== Section 2: Money Supply (FRED) ===")

money_specs <- list(
  us_m2 = list(
    series_id = "M2SL",
    country   = "US",
    variable  = "m2",
    notes     = "US M2 money supply, seasonally adjusted, billions USD"
  ),
  jp_m2 = list(
    series_id = "MYAGM2JPM189N",
    country   = "JP",
    variable  = "m2",
    notes     = "Japan M2, yen billions"
  ),
  gb_m4 = list(
    series_id = "MABMM301GBM189S",
    country   = "GB",
    variable  = "m4_broad",
    notes     = "UK M4 broad money, millions GBP"
  )
)

fetch_money_series <- function(spec, name) {
  msid <- spec$series_id
  log_info("FRED money: {name} ({msid})")
  raw <- safe_fetch(
    fredr(
      series_id          = msid,
      observation_start  = as.Date(STUDY_START),
      observation_end    = as.Date(STUDY_END),
      frequency          = "m",
      aggregation_method = "avg"
    ),
    source_name = "FRED_money",
    series_id   = spec$series_id
  )
  if (is.null(raw)) return(NULL)

  save_raw(raw, "fred", glue("money_{name}"))

  mvar    <- spec$variable
  mcountry <- spec$country
  raw |>
    transmute(
      date    = date,
      country = mcountry,
      !!mvar := value
    ) |>
    filter(!is.na(.data[[mvar]]))
}

money_results <- imap(money_specs, fetch_money_series)
money_ok      <- keep(money_results, ~ !is.null(.x))

AVAILABILITY[["money_supply"]] <- list(
  attempted = names(money_specs),
  succeeded = names(money_ok),
  failed    = setdiff(names(money_specs), names(money_ok))
)

if (length(money_ok) > 0) {
  money_long <- bind_rows(money_ok) |>
    arrange(country, date)

  save_processed(money_long, "money_supply_monthly")
  log_info("Money supply: {nrow(money_long)} rows for {n_distinct(money_long$country)} countries")
} else {
  log_warn("No money supply data collected")
}

# =============================================================================
# Section 3: Chinn-Ito Financial Openness Index
# =============================================================================
# Primary URL: https://web.pdx.edu/~ito/Chinn-Ito_website_2022.xlsx
# If 404, scrape the page for the current XLS link via rvest.
# Target columns: country code (IMF or ISO) + kaopen (financial openness index).
# =============================================================================

log_info("=== Section 3: Chinn-Ito Financial Openness ===")

# IMF country codes -> our 2-letter codes (Chinn-Ito uses IMF codes)
chinnito_imf_codes <- c(
  "111" = "US",   # United States
  "142" = "NO",   # Norway
  "144" = "SE",   # Sweden
  "128" = "DK",   # Denmark
  "158" = "JP",   # Japan
  "112" = "GB",   # United Kingdom
  "146" = "CH"    # Switzerland
)

# ISO3 codes -> our 2-letter codes (fallback if IMF codes not in file)
chinnito_iso3_codes <- c(
  "USA" = "US",
  "NOR" = "NO",
  "SWE" = "SE",
  "DNK" = "DK",
  "JPN" = "JP",
  "GBR" = "GB",
  "CHE" = "CH"
)

chinnito_primary_url <- "https://web.pdx.edu/~ito/Chinn-Ito_website_2022.xlsx"
chinnito_page_url    <- "https://web.pdx.edu/~ito/Chinn-Ito_website.htm"
# destfile extension set later based on actual URL (may be .xls or .xlsx)
chinnito_destfile    <- file.path(DIR_RAW_CHINNITO, "chinn_ito.xlsx")

# Try primary URL first
chinnito_download_ok <- safe_fetch(
  {
    resp <- httr::GET(chinnito_primary_url, httr::timeout(60),
                      httr::write_disk(chinnito_destfile, overwrite = TRUE))
    if (httr::http_error(resp)) stop(paste("HTTP", httr::status_code(resp)))
    TRUE
  },
  source_name = "Chinn-Ito",
  series_id   = "primary URL"
)

# If primary fails, scrape page for current link
if (is.null(chinnito_download_ok)) {
  log_warn("Chinn-Ito primary URL failed. Attempting page scrape: {chinnito_page_url}")

  scraped_url <- safe_fetch(
    {
      page  <- rvest::read_html(chinnito_page_url)
      links <- page |>
        rvest::html_elements("a") |>
        rvest::html_attr("href")
      # Find link ending in .xlsx or .xls
      xls_link <- links[grepl("\\.(xlsx?|XLS?)$", links, ignore.case = TRUE)][1]
      if (is.na(xls_link)) stop("No XLS link found on Chinn-Ito page")
      # Make absolute if relative
      if (!grepl("^https?://", xls_link)) {
        base_url <- "https://web.pdx.edu/~ito/"
        xls_link <- paste0(base_url, xls_link)
      }
      log_info("Chinn-Ito scraped link: {xls_link}")
      xls_link
    },
    source_name = "Chinn-Ito",
    series_id   = "page scrape"
  )

  if (!is.null(scraped_url)) {
    # Match extension from actual URL (.xls or .xlsx)
    ext <- ifelse(grepl("\\.xlsx$", scraped_url, ignore.case = TRUE), ".xlsx", ".xls")
    chinnito_destfile <- file.path(DIR_RAW_CHINNITO, paste0("chinn_ito", ext))
    chinnito_download_ok <- safe_fetch(
      {
        resp <- httr::GET(scraped_url, httr::timeout(60),
                          httr::write_disk(chinnito_destfile, overwrite = TRUE))
        if (httr::http_error(resp)) stop(paste("HTTP", httr::status_code(resp)))
        TRUE
      },
      source_name = "Chinn-Ito",
      series_id   = "scraped URL"
    )
  }
}

if (!is.null(chinnito_download_ok) && file.exists(chinnito_destfile)) {
  log_info("Chinn-Ito file at {chinnito_destfile}")

  # Read all sheets to find the data
  sheets <- safe_fetch(
    readxl::excel_sheets(chinnito_destfile),
    source_name = "Chinn-Ito",
    series_id   = "sheet names"
  )
  log_info("Chinn-Ito sheets: {paste(sheets, collapse=', ')}")

  # Read first sheet (data is usually on sheet 1)
  chinnito_raw <- safe_fetch(
    readxl::read_excel(chinnito_destfile, sheet = 1),
    source_name = "Chinn-Ito",
    series_id   = "sheet 1"
  )

  if (!is.null(chinnito_raw)) {
    save_raw(chinnito_raw, "chinnito", "chinn_ito_raw")
    log_info("Chinn-Ito raw: {nrow(chinnito_raw)} rows, columns: {paste(names(chinnito_raw)[1:min(10,ncol(chinnito_raw))], collapse=', ')}")

    col_names_lower <- tolower(names(chinnito_raw))

    # Find kaopen column (the financial openness index)
    kaopen_col <- which(grepl("kaopen", col_names_lower))[1]
    if (is.na(kaopen_col)) {
      kaopen_col <- which(grepl("open|kaopenx", col_names_lower))[1]
    }

    # Find year column
    year_col <- which(grepl("^year$|^yr$", col_names_lower))[1]
    if (is.na(year_col)) year_col <- which(grepl("year|yr", col_names_lower))[1]

    # Detect country identifier columns
    # In kaopen_2023.xls: cn = IMF numeric code, ccode = ISO3 code
    cn_col   <- which(col_names_lower == "cn")[1]
    iso3_col <- which(col_names_lower == "ccode")[1]
    name_col <- which(grepl("country_name|countryname|^country$", col_names_lower))[1]

    log_info(
      "Chinn-Ito column detection — year:{year_col}, kaopen:{kaopen_col}, ",
      "cn:{cn_col}, ccode:{iso3_col}"
    )

    if (is.na(kaopen_col) || is.na(year_col)) {
      log_error("Chinn-Ito: cannot find kaopen or year column. Check file structure.")
      AVAILABILITY[["chinnito"]] <- list(status = "column_detection_failed")
    } else {
      chinnito_work <- chinnito_raw |>
        rename(
          year   = all_of(year_col),
          kaopen = all_of(kaopen_col)
        ) |>
        mutate(
          year   = suppressWarnings(as.integer(year)),
          kaopen = suppressWarnings(as.numeric(kaopen))
        ) |>
        filter(!is.na(year), !is.na(kaopen), year >= 2004)

      # Try IMF numeric codes first (cn column), then ISO3 (ccode), then ISO2 guess
      if (!is.na(cn_col)) {
        cn_name <- names(chinnito_raw)[cn_col]
        cn_vals <- trimws(as.character(chinnito_work[[cn_name]]))
        # Detect if cn holds IMF codes (numeric) or ISO codes (letters)
        is_numeric_code <- mean(grepl("^[0-9]+$", cn_vals[cn_vals != ""])) > 0.5
        if (is_numeric_code) {
          chinnito_work <- chinnito_work |>
            mutate(country = chinnito_imf_codes[trimws(as.character(.data[[cn_name]]))])
          log_info("Chinn-Ito: using cn (IMF numeric codes) for matching")
        } else {
          chinnito_work <- chinnito_work |>
            mutate(
              iso2    = toupper(trimws(as.character(.data[[cn_name]]))),
              country = case_when(
                iso2 %in% c("US","USA") ~ "US", iso2 %in% c("NO","NOR") ~ "NO",
                iso2 %in% c("SE","SWE") ~ "SE", iso2 %in% c("DK","DNK") ~ "DK",
                iso2 %in% c("JP","JPN") ~ "JP", iso2 %in% c("GB","GBR") ~ "GB",
                iso2 %in% c("CH","CHE") ~ "CH", TRUE ~ NA_character_
              )
            )
          log_info("Chinn-Ito: using cn (ISO alpha) for matching")
        }
      } else if (!is.na(iso3_col)) {
        iso3_name <- names(chinnito_raw)[iso3_col]
        chinnito_work <- chinnito_work |>
          mutate(
            iso3    = toupper(trimws(as.character(.data[[iso3_name]]))),
            country = chinnito_iso3_codes[iso3]
          )
        log_info("Chinn-Ito: using ccode (ISO3) for matching")
      } else {
        log_error("Chinn-Ito: no usable country identifier column found")
        chinnito_work <- chinnito_work |> mutate(country = NA_character_)
      }

      chinnito_output <- chinnito_work |>
        filter(!is.na(country)) |>
        select(country, year, kaopen) |>
        arrange(country, year)

      target_countries <- setdiff(COUNTRIES, "EA")  # Chinn-Ito has no EA aggregate
      found_countries  <- unique(chinnito_output$country)
      missing_countries <- setdiff(target_countries, found_countries)

      log_info("Chinn-Ito: found {n_distinct(chinnito_output$country)} target countries: {paste(found_countries, collapse=', ')}")
      if (length(missing_countries) > 0) {
        log_warn("Chinn-Ito: missing countries: {paste(missing_countries, collapse=', ')}")
      }

      save_processed(chinnito_output, "chinnito_annual")

      AVAILABILITY[["chinnito"]] <- list(
        status    = "ok",
        rows      = nrow(chinnito_output),
        countries = found_countries,
        missing   = missing_countries
      )
    }
  } else {
    log_error("Chinn-Ito: failed to parse Excel file")
    AVAILABILITY[["chinnito"]] <- list(status = "parse_failed")
  }
} else {
  log_error("Chinn-Ito: download failed from both primary URL and scraped link")
  AVAILABILITY[["chinnito"]] <- list(status = "download_failed")
}

# =============================================================================
# Save availability record for this script
# =============================================================================

saveRDS(AVAILABILITY, file.path(DIR_PROCESSED, "availability_03_controls.rds"))
log_info("Script 03 complete. Availability saved.")
