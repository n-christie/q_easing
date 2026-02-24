# =============================================================================
# 01_collect_macro.R — CB Balance Sheets, GDP, CPI, Yields
# Sources: BIS CBTA API, FRED, World Bank
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(httr)
  library(fredr)
  library(wbstats)
  library(lubridate)
})

fredr_set_key(FRED_API_KEY)

# =============================================================================
# Section 1: BIS CBTA API — Central Bank Total Assets (primary source)
# =============================================================================
# Dataflow: WS_CBTA (not CBTA — confirmed via BIS dataflow registry)
# URL: https://stats.bis.org/api/v1/data/WS_CBTA?startPeriod=2005-01&format=csv
# Fetches full dataset; filter by REF_AREA column in R.
# Units: UNIT_MULT=9 (billions), UNIT_MEASURE=USD — data is already in billion USD.
# Columns: FREQ, REF_AREA, COMP_METHOD, UNIT_MEASURE, CURRENCY, TRANSFORMATION,
#          TIME_PERIOD, OBS_VALUE, ...
# =============================================================================

log_info("=== Section 1: BIS CBTA ===")

# BIS REF_AREA codes that match our country codes (BIS uses same 2-letter for most)
BIS_REF_AREAS <- c(US = "US", EA = "XM", NO = "NO", SE = "SE",
                   DK = "DK", JP = "JP", GB = "GB", CH = "CH")

bis_url <- paste0(
  "https://stats.bis.org/api/v1/data/WS_CBTA",
  "?startPeriod=2005-01&format=csv"
)

log_info("BIS CBTA: fetching full dataset")
bis_resp <- safe_fetch(
  httr::GET(bis_url, httr::timeout(120)),
  source_name = "BIS_CBTA",
  series_id   = "WS_CBTA full"
)

bis_cbta_raw <- list()

if (!is.null(bis_resp) && !httr::http_error(bis_resp)) {
  bis_raw_text <- httr::content(bis_resp, "text", encoding = "UTF-8")
  save_raw(bis_raw_text, "bis", "cbta_all_countries")

  bis_df <- tryCatch(
    read_csv(I(bis_raw_text), show_col_types = FALSE),
    error = function(e) {
      log_error("BIS CBTA parse failed: {conditionMessage(e)}")
      NULL
    }
  )

  if (!is.null(bis_df) && nrow(bis_df) > 0) {
    log_info("BIS CBTA raw: {nrow(bis_df)} rows, columns: {paste(names(bis_df)[1:8], collapse=', ')}")

    # Filter to USD series, monthly, our countries, total assets method (B = BIS-spliced)
    # Units: UNIT_MULT = 9 -> billions USD; UNIT_MEASURE = USD
    bis_filtered <- bis_df |>
      filter(
        FREQ        == "M",
        UNIT_MEASURE == "USD",
        REF_AREA    %in% unname(BIS_REF_AREAS)
      ) |>
      mutate(
        date      = lubridate::ym(TIME_PERIOD),
        cb_assets = suppressWarnings(as.numeric(OBS_VALUE)),  # already in billion USD
        source    = "BIS_CBTA_USD"
      ) |>
      filter(!is.na(date), !is.na(cb_assets))

    # Map REF_AREA back to our country codes
    area_to_country <- setNames(names(BIS_REF_AREAS), BIS_REF_AREAS)
    bis_filtered <- bis_filtered |>
      mutate(country = area_to_country[REF_AREA]) |>
      select(date, country, cb_assets, source, COMP_METHOD) |>
      # If multiple COMP_METHOD per country/date, prefer BIS-spliced (B) then others
      group_by(country, date) |>
      arrange(desc(COMP_METHOD == "B")) |>
      slice(1) |>
      ungroup() |>
      select(date, country, cb_assets, source)

    for (cc in names(BIS_REF_AREAS)) {
      ctry_data <- filter(bis_filtered, country == cc)
      if (nrow(ctry_data) > 0) {
        bis_cbta_raw[[cc]] <- ctry_data
        log_info("BIS CBTA: {cc} — {nrow(ctry_data)} rows ({min(ctry_data$date)} to {max(ctry_data$date)})")
      } else {
        log_warn("BIS CBTA: no data for {cc}")
      }
    }
  }
} else {
  if (!is.null(bis_resp)) {
    log_error("BIS CBTA HTTP error: {httr::status_code(bis_resp)}")
  }
}

AVAILABILITY[["bis_cbta"]] <- list(
  attempted = names(BIS_REF_AREAS),
  succeeded = names(bis_cbta_raw),
  failed    = setdiff(names(BIS_REF_AREAS), names(bis_cbta_raw)),
  note      = "Units: billion USD (BIS pre-converted)"
)
log_info("BIS CBTA: {length(bis_cbta_raw)}/{length(BIS_REF_AREAS)} countries. Failed: {paste(AVAILABILITY$bis_cbta$failed, collapse=', ')}")

# =============================================================================
# Section 2: FRED CB Asset Fallbacks (only if BIS failed for US / JP / GB)
# =============================================================================

log_info("=== Section 2: FRED CB Fallbacks ===")

fred_cb_specs <- list(
  US = list(series_id = "WALCL",      freq = "m", agg = "avg", label = "Fed total assets (millions USD)"),
  JP = list(series_id = "JPNASSETS",  freq = "m", agg = "avg", label = "BoJ total assets"),
  GB = list(series_id = "BOEBSTAUKA", freq = "a", agg = "avg", label = "BoE total assets (annual only)")
)

fetch_fred_cb_fallback <- function(spec, country) {
  if (country %in% names(bis_cbta_raw)) {
    log_info("FRED CB fallback skipped for {country} - BIS data available")
    return(NULL)
  }

  sid <- spec$series_id
  log_info("FRED CB fallback: {country} - {sid}")
  raw <- safe_fetch(
    fredr(
      series_id          = sid,
      observation_start  = as.Date(STUDY_START),
      observation_end    = as.Date(STUDY_END),
      frequency          = spec$freq,
      aggregation_method = spec$agg
    ),
    source_name = "FRED_CB",
    series_id   = sid
  )
  if (is.null(raw)) return(NULL)

  save_raw(raw, "fred", glue("cb_{country}_{sid}"))

  raw |>
    transmute(
      date      = date,
      country   = country,
      cb_assets = value,
      source    = glue("FRED_{sid}")
    ) |>
    filter(!is.na(cb_assets))
}

fred_cb_results <- imap(fred_cb_specs, fetch_fred_cb_fallback)
fred_cb_raw     <- keep(fred_cb_results, ~ !is.null(.x))

AVAILABILITY[["fred_cb_fallback"]] <- list(
  attempted = names(fred_cb_specs)[!(names(fred_cb_specs) %in% names(bis_cbta_raw))],
  succeeded = names(fred_cb_raw)
)

# Combine BIS + FRED fallback CB assets
all_cb_long <- bind_rows(
  bind_rows(bis_cbta_raw),
  bind_rows(fred_cb_raw)
)

if (nrow(all_cb_long) > 0) {
  save_processed(all_cb_long, "bis_cbta_monthly")
  log_info("CB assets combined: {nrow(all_cb_long)} rows, {n_distinct(all_cb_long$country)} countries")
} else {
  log_warn("No CB asset data collected from BIS or FRED fallback")
}

# =============================================================================
# Section 3: FRED Macro Series
# =============================================================================

log_info("=== Section 3: FRED Macro Series ===")

fred_macro_specs <- list(
  us_gdp_quarterly = list(series_id = "GDP",              freq = "q", agg = "avg", notes = "US GDP quarterly, billions USD"),
  us_cpi           = list(series_id = "CPIAUCSL",         freq = "m", agg = "avg", notes = "US CPI all items, index"),
  us_10y           = list(series_id = "GS10",             freq = "m", agg = "avg", notes = "US 10-year Treasury yield"),
  vix              = list(series_id = "VIXCLS",           freq = "m", agg = "avg", notes = "VIX, daily->monthly avg"),
  fx_nok_usd       = list(series_id = "DEXNOUS",         freq = "m", agg = "avg", notes = "NOK per USD"),
  fx_sek_usd       = list(series_id = "DEXSDUS",         freq = "m", agg = "avg", notes = "SEK per USD"),
  fx_dkk_usd       = list(series_id = "DEXDNUS",         freq = "m", agg = "avg", notes = "DKK per USD"),
  fx_eur_usd       = list(series_id = "DEXUSEU",         freq = "m", agg = "avg", notes = "USD per EUR"),
  fx_jpy_usd       = list(series_id = "DEXJPUS",         freq = "m", agg = "avg", notes = "JPY per USD"),
  fx_gbp_usd       = list(series_id = "DEXUSUK",         freq = "m", agg = "avg", notes = "USD per GBP"),
  fx_chf_usd       = list(series_id = "DEXSZUS",         freq = "m", agg = "avg", notes = "CHF per USD"),
  # OECD 10-year government bond yields (% per annum)
  yield_no         = list(series_id = "IRLTLT01NOM156N", freq = "m", agg = "avg", notes = "Norway 10Y govt bond yield (OECD)"),
  yield_se         = list(series_id = "IRLTLT01SEM156N", freq = "m", agg = "avg", notes = "Sweden 10Y govt bond yield (OECD)"),
  yield_dk         = list(series_id = "IRLTLT01DKM156N", freq = "m", agg = "avg", notes = "Denmark 10Y govt bond yield (OECD)"),
  yield_jp         = list(series_id = "IRLTLT01JPM156N", freq = "m", agg = "avg", notes = "Japan 10Y govt bond yield (OECD)"),
  yield_gb         = list(series_id = "IRLTLT01GBM156N", freq = "m", agg = "avg", notes = "UK 10Y govt bond yield (OECD)"),
  yield_ch         = list(series_id = "IRLTLT01CHM156N", freq = "m", agg = "avg", notes = "Switzerland 10Y govt bond yield (OECD)"),
  # OECD CPI indices (2015=100) — YoY computed in 04_clean_merge.R
  cpi_no           = list(series_id = "CPALTT01NOM657N", freq = "m", agg = "avg", notes = "Norway CPI index (OECD, 2015=100)"),
  cpi_se           = list(series_id = "CPALTT01SEM657N", freq = "m", agg = "avg", notes = "Sweden CPI index (OECD, 2015=100)"),
  cpi_dk           = list(series_id = "CPALTT01DKM657N", freq = "m", agg = "avg", notes = "Denmark CPI index (OECD, 2015=100)"),
  cpi_jp           = list(series_id = "CPALTT01JPM657N", freq = "m", agg = "avg", notes = "Japan CPI index (OECD, 2015=100)"),
  cpi_gb           = list(series_id = "CPALTT01GBM657N", freq = "m", agg = "avg", notes = "UK CPI index (OECD, 2015=100)"),
  cpi_ch           = list(series_id = "CPALTT01CHM657N", freq = "m", agg = "avg", notes = "Switzerland CPI index (OECD, 2015=100)")
)

fetch_fred_series <- function(spec, name) {
  sid <- spec$series_id
  log_info("FRED macro: {name} ({sid})")
  raw <- safe_fetch(
    fredr(
      series_id          = sid,
      observation_start  = as.Date(STUDY_START),
      observation_end    = as.Date(STUDY_END),
      frequency          = spec$freq,
      aggregation_method = spec$agg
    ),
    source_name = "FRED",
    series_id   = sid
  )
  if (is.null(raw)) return(NULL)

  save_raw(raw, "fred", name)

  raw |>
    transmute(
      date     = date,
      series   = name,
      variable = sid,
      value    = value
    ) |>
    filter(!is.na(value))
}

fred_macro_results <- imap(fred_macro_specs, fetch_fred_series)
fred_macro_raw     <- keep(fred_macro_results, ~ !is.null(.x))

AVAILABILITY[["fred_macro"]] <- list(
  attempted = names(fred_macro_specs),
  succeeded = names(fred_macro_raw),
  failed    = setdiff(names(fred_macro_specs), names(fred_macro_raw))
)
log_info("FRED macro: {length(fred_macro_raw)}/{length(fred_macro_specs)} series. Failed: {paste(AVAILABILITY$fred_macro$failed, collapse=', ')}")

# Pivot to wide: one row per date, one column per series
if (length(fred_macro_raw) > 0) {
  fred_macro_long <- bind_rows(fred_macro_raw)
  fred_macro_wide <- fred_macro_long |>
    select(date, series, value) |>
    pivot_wider(names_from = series, values_from = value) |>
    arrange(date)

  save_processed(fred_macro_wide, "fred_macro_monthly")
  log_info("FRED macro wide: {nrow(fred_macro_wide)} rows, {ncol(fred_macro_wide)-1} series")
} else {
  log_warn("No FRED macro series collected")
  fred_macro_wide <- tibble(date = as.Date(character()))
}

# =============================================================================
# Section 4: World Bank GDP (annual, nominal USD)
# =============================================================================

log_info("=== Section 4: World Bank GDP ===")

wb_raw <- safe_fetch(
  wbstats::wb_data(
    indicator  = "NY.GDP.MKTP.CD",
    country    = unname(WB_COUNTRY_CODES),
    start_date = 2004,
    end_date   = as.integer(format(Sys.Date(), "%Y"))
  ),
  source_name = "WorldBank",
  series_id   = "NY.GDP.MKTP.CD"
)

if (!is.null(wb_raw)) {
  save_raw(wb_raw, "worldbank", "gdp_nominal_usd")

  wb_to_country <- setNames(names(WB_COUNTRY_CODES), WB_COUNTRY_CODES)

  wb_gdp <- wb_raw |>
    transmute(
      iso3c              = iso3c,
      country            = wb_to_country[iso3c],
      year               = date,
      gdp_nominal_usd_wb = NY.GDP.MKTP.CD
    ) |>
    filter(!is.na(country), !is.na(gdp_nominal_usd_wb)) |>
    arrange(country, year)

  save_processed(wb_gdp, "wb_gdp_annual")
  log_info("World Bank GDP: {nrow(wb_gdp)} rows for {n_distinct(wb_gdp$country)} countries")
  AVAILABILITY[["wb_gdp"]] <- list(
    attempted = names(WB_COUNTRY_CODES),
    succeeded = unique(wb_gdp$country),
    failed    = setdiff(names(WB_COUNTRY_CODES), unique(wb_gdp$country))
  )
} else {
  log_warn("World Bank GDP collection failed")
  AVAILABILITY[["wb_gdp"]] <- list(
    attempted = names(WB_COUNTRY_CODES),
    succeeded = character(0),
    failed    = names(WB_COUNTRY_CODES)
  )
}

# =============================================================================
# Save availability record for this script
# =============================================================================

saveRDS(AVAILABILITY, file.path(DIR_PROCESSED, "availability_01_macro.rds"))
log_info("Script 01 complete. Availability saved.")
