# =============================================================================
# 04_clean_merge.R — Harmonize, Merge, Construct Variables
# Produces: panel_monthly.{rds,csv}, panel_quarterly.{rds,csv},
#           coverage_summary.{rds,csv}
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(lubridate)
  library(zoo)
})

PANEL_START <- as.Date(STUDY_START)
PANEL_END   <- floor_date(as.Date(STUDY_END), "month")

# =============================================================================
# Load processed data
# =============================================================================

log_info("=== Loading processed data ===")

load_if_exists <- function(filename_stem) {
  path <- file.path(DIR_PROCESSED, paste0(filename_stem, ".rds"))
  if (file.exists(path)) {
    readRDS(path)
  } else {
    log_warn("File not found: {path}")
    NULL
  }
}

cb_assets_raw  <- load_if_exists("bis_cbta_monthly")
fred_macro     <- load_if_exists("fred_macro_monthly")
wb_gdp         <- load_if_exists("wb_gdp_annual")
shiller_cape   <- load_if_exists("shiller_cape_us")
ecb_macro      <- load_if_exists("ecb_macro_monthly")
money_supply   <- load_if_exists("money_supply_monthly")
chinnito       <- load_if_exists("chinnito_annual")

# Manual files (optional, populated by researcher)
manual_cb_no   <- {
  p <- file.path(DIR_MANUAL, "cb_assets_NO.csv")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) |> mutate(country = "NO", source = "Manual_SSB") else NULL
}
manual_cb_se   <- {
  p <- file.path(DIR_MANUAL, "cb_assets_SE.csv")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) |> mutate(country = "SE", source = "Manual_Riksbank") else NULL
}
manual_cb_dk   <- {
  p <- file.path(DIR_MANUAL, "cb_assets_DK.csv")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) |> mutate(country = "DK", source = "Manual_Nationalbanken") else NULL
}
manual_equity  <- {
  p <- file.path(DIR_MANUAL, "equity_valuation.csv")
  if (file.exists(p)) read_csv(p, show_col_types = FALSE) else NULL
}

if (!is.null(manual_cb_no))  log_info("Manual CB assets: NO loaded ({nrow(manual_cb_no)} rows)")
if (!is.null(manual_cb_se))  log_info("Manual CB assets: SE loaded ({nrow(manual_cb_se)} rows)")
if (!is.null(manual_cb_dk))  log_info("Manual CB assets: DK loaded ({nrow(manual_cb_dk)} rows)")
if (!is.null(manual_equity)) log_info("Manual equity valuation loaded ({nrow(manual_equity)} rows)")

# =============================================================================
# Step 1: Panel spine — all (country, date) combinations
# =============================================================================

log_info("=== Building panel spine ===")

spine <- expand_grid(
  country = COUNTRIES,
  date    = seq.Date(PANEL_START, PANEL_END, by = "month")
) |>
  mutate(
    year    = year(date),
    month   = month(date),
    quarter = quarter(date)
  )

log_info("Panel spine: {nrow(spine)} rows ({length(COUNTRIES)} countries x {n_distinct(spine$date)} months)")

# =============================================================================
# Step 2: CB Assets consolidation
# =============================================================================

log_info("=== Consolidating CB assets ===")

# Combine BIS/FRED + manual sources, prefer BIS where both exist
cb_all <- bind_rows(
  cb_assets_raw,
  manual_cb_no,
  manual_cb_se,
  manual_cb_dk
)

if (!is.null(cb_all) && nrow(cb_all) > 0) {
  # Standardise date to first of month
  cb_all <- cb_all |>
    mutate(date = floor_date(as.Date(date), "month")) |>
    # BIS source takes priority within same country/date
    arrange(country, date, desc(grepl("^BIS|^FRED", source))) |>
    group_by(country, date) |>
    slice(1) |>
    ungroup() |>
    rename(cb_assets_local = cb_assets, cb_source = source)
} else {
  log_warn("No CB asset data available")
  cb_all <- tibble(
    country         = character(),
    date            = as.Date(character()),
    cb_assets_local = numeric(),
    cb_source       = character()
  )
}

# =============================================================================
# Step 3: Currency conversion — CB assets to USD
# =============================================================================
# BIS CBTA units: billions of national currency.
# Manual files: millions of national currency (per README spec).
# FRED fallback (WALCL): millions USD, so no conversion needed.
# =============================================================================

log_info("=== Currency conversion for CB assets ===")

# FX rates from FRED macro (monthly averages)
# Convention: amount_USD = amount_local / fx_local_per_usd
#   NOK: fx_nok_usd = NOK per USD  → usd = nok / fx_nok_usd
#   SEK: fx_sek_usd = SEK per USD  → usd = sek / fx_sek_usd
#   DKK: fx_dkk_usd = DKK per USD  → usd = dkk / fx_dkk_usd
#   EUR: fx_eur_usd = USD per EUR   → usd = eur * fx_eur_usd
#   JPY: fx_jpy_usd = JPY per USD   → usd = jpy / fx_jpy_usd
#   GBP: fx_gbp_usd = USD per GBP   → usd = gbp * fx_gbp_usd
#   CHF: fx_chf_usd = CHF per USD   → usd = chf / fx_chf_usd
#   USD: no conversion

fx_wide <- if (!is.null(fred_macro)) {
  fred_macro |>
    select(date, starts_with("fx_")) |>
    mutate(date = floor_date(as.Date(date), "month"))
} else {
  log_warn("FRED macro not available — FX conversion will produce NAs")
  tibble(date = as.Date(character()))
}

convert_cb_to_usd <- function(df, fx) {
  df |>
    left_join(fx, by = "date") |>
    mutate(
      # Scale factor: BIS = billions local, Manual = millions local → unify to millions local first
      local_millions = case_when(
        grepl("^BIS|^FRED", cb_source) & !grepl("WALCL", cb_source) ~ cb_assets_local * 1000,  # billions -> millions
        grepl("WALCL", cb_source) ~ cb_assets_local,        # FRED WALCL already millions USD
        TRUE                       ~ cb_assets_local         # manual files: millions local
      ),
      # Convert to USD millions
      cb_assets_usd_millions = case_when(
        country == "US" & grepl("WALCL", cb_source) ~ local_millions,           # already USD
        country == "US"  ~ local_millions,                                       # USD, no conversion
        country == "EA"  ~ local_millions * fx_eur_usd,                         # EUR -> USD
        country == "NO"  ~ local_millions / fx_nok_usd,
        country == "SE"  ~ local_millions / fx_sek_usd,
        country == "DK"  ~ local_millions / fx_dkk_usd,
        country == "JP"  ~ local_millions / fx_jpy_usd,
        country == "GB"  ~ local_millions * fx_gbp_usd,
        country == "CH"  ~ local_millions / fx_chf_usd,
        TRUE             ~ NA_real_
      ),
      cb_assets = cb_assets_usd_millions  # final column name
    ) |>
    select(country, date, cb_assets_local, cb_assets, cb_source)
}

cb_converted <- if (nrow(cb_all) > 0) {
  convert_cb_to_usd(cb_all, fx_wide)
} else {
  cb_all |> mutate(cb_assets = NA_real_)
}

# =============================================================================
# Step 4: GDP — interpolate quarterly/annual to monthly
# =============================================================================

log_info("=== GDP interpolation to monthly ===")

# US GDP from FRED (quarterly, frequency="q", date = quarter start)
us_gdp_fred <- if (!is.null(fred_macro) && "us_gdp_quarterly" %in% names(fred_macro)) {
  fred_macro |>
    filter(!is.na(us_gdp_quarterly)) |>
    transmute(
      country = "US",
      # FRED quarter-start dates -> quarter-end month for interpolation anchor
      date    = floor_date(date %m+% months(2), "month"),
      gdp_q   = us_gdp_quarterly * 4e9  # annualised billions USD -> annual USD
    )
} else NULL

# World Bank annual GDP (nominal USD)
# Convention: assign to July 1 (mid-year)
wb_gdp_monthly_anchor <- if (!is.null(wb_gdp)) {
  wb_gdp |>
    mutate(date = make_date(year, 7, 1)) |>
    select(country, date, gdp_nominal_usd_wb)
} else NULL

interpolate_gdp_to_monthly <- function(country_code, spine_dates, gdp_data, gdp_col) {
  if (is.null(gdp_data)) return(tibble(date = spine_dates, !!gdp_col := NA_real_))

  ctry_data <- gdp_data |> filter(country == country_code)
  if (nrow(ctry_data) == 0) {
    return(tibble(date = spine_dates, !!gdp_col := NA_real_))
  }

  # Create zoo series on observed dates
  obs_z <- zoo(ctry_data[[gdp_col]], ctry_data$date)

  # Full monthly series
  all_dates_z <- zoo(NA_real_, spine_dates)

  # Merge observed with full monthly, then spline interpolate
  merged_z <- merge(obs_z, all_dates_z, all = TRUE)
  merged_z[, 1][is.na(merged_z[, 1])] <- merged_z[, 2][is.na(merged_z[, 1])]

  interp_z <- tryCatch(
    zoo::na.spline(merged_z[, 1]),
    error = function(e) {
      log_warn("GDP spline failed for {country_code}: {conditionMessage(e)}. Using linear fill.")
      zoo::na.approx(merged_z[, 1], na.rm = FALSE)
    }
  )

  tibble(
    date       = spine_dates,
    !!gdp_col := as.numeric(window(interp_z, start = min(spine_dates), end = max(spine_dates)))
  )
}

# Interpolate WB GDP for all countries
spine_dates <- sort(unique(spine$date))

gdp_interp_list <- map(COUNTRIES, function(cc) {
  interp <- interpolate_gdp_to_monthly(cc, spine_dates, wb_gdp_monthly_anchor, "gdp_nominal_usd_wb")
  interp |> mutate(country = cc)
})
gdp_interp <- bind_rows(gdp_interp_list)

# For US, prefer FRED quarterly (finer anchor), blend if available
if (!is.null(us_gdp_fred)) {
  us_gdp_interp_fred <- interpolate_gdp_to_monthly("US", spine_dates, us_gdp_fred, "gdp_q")

  gdp_interp <- gdp_interp |>
    left_join(
      us_gdp_interp_fred |> mutate(country = "US"),
      by = c("country", "date")
    ) |>
    mutate(
      # Prefer FRED quarterly for US; WB as fallback
      gdp_nominal_usd = case_when(
        country == "US" & !is.na(gdp_q)          ~ gdp_q,
        country == "US" & !is.na(gdp_nominal_usd_wb) ~ gdp_nominal_usd_wb,
        TRUE                                          ~ gdp_nominal_usd_wb
      )
    ) |>
    select(-gdp_q)
} else {
  gdp_interp <- gdp_interp |> rename(gdp_nominal_usd = gdp_nominal_usd_wb)
}

log_info("GDP interpolated: {nrow(gdp_interp)} rows for {n_distinct(gdp_interp$country)} countries")

# =============================================================================
# Step 5: Equity valuation — combine Shiller (US) + manual (non-US)
# =============================================================================

log_info("=== Equity valuation ===")

equity_us <- if (!is.null(shiller_cape)) {
  shiller_cape |> mutate(date = floor_date(date, "month"))
} else NULL

equity_all <- bind_rows(equity_us, manual_equity)

if (!is.null(equity_all) && nrow(equity_all) > 0) {
  equity_all <- equity_all |>
    mutate(date = floor_date(as.Date(date), "month")) |>
    group_by(country, date) |>
    slice(1) |>
    ungroup()
  log_info("Equity valuation: {nrow(equity_all)} rows for {n_distinct(equity_all$country)} countries")
} else {
  log_warn("No equity valuation data available")
  equity_all <- tibble(
    date     = as.Date(character()),
    country  = character(),
    cape     = numeric(),
    pe_ratio = numeric()
  )
}

# =============================================================================
# Step 6: Join all components onto the spine
# =============================================================================

log_info("=== Assembling panel ===")

panel <- spine |>
  left_join(cb_converted |> select(country, date, cb_assets, cb_assets_local, cb_source),
            by = c("country", "date")) |>
  left_join(gdp_interp, by = c("country", "date")) |>
  left_join(equity_all |> select(country, date, cape, pe_ratio),
            by = c("country", "date"))

# ECB macro (EA only) — conditional join outside pipe (R 4.1 doesn't support {} in |>)
if (!is.null(ecb_macro)) {
  ecb_join <- ecb_macro |>
    mutate(date = floor_date(date, "month")) |>
    select(date,
           cpi_yoy_ea   = any_of("cpi_yoy"),
           yield_10y_ea = any_of("yield_10y"),
           m3_ea        = any_of("m3"))
  panel <- left_join(panel, ecb_join, by = "date")
}

# FRED macro (global)
if (!is.null(fred_macro)) {
  fred_join <- fred_macro |>
    mutate(date = floor_date(as.Date(date), "month")) |>
    select(date,
           us_cpi     = any_of("us_cpi"),
           us_10y     = any_of("us_10y"),
           vix        = any_of("vix"),
           fx_nok_usd = any_of("fx_nok_usd"),
           fx_sek_usd = any_of("fx_sek_usd"),
           fx_dkk_usd = any_of("fx_dkk_usd"),
           fx_eur_usd = any_of("fx_eur_usd"),
           fx_jpy_usd = any_of("fx_jpy_usd"),
           fx_gbp_usd = any_of("fx_gbp_usd"),
           fx_chf_usd = any_of("fx_chf_usd"),
           # OECD per-country 10Y yields
           yield_no   = any_of("yield_no"),
           yield_se   = any_of("yield_se"),
           yield_dk   = any_of("yield_dk"),
           yield_jp   = any_of("yield_jp"),
           yield_gb   = any_of("yield_gb"),
           yield_ch   = any_of("yield_ch"),
           # OECD per-country CPI indices
           cpi_no     = any_of("cpi_no"),
           cpi_se     = any_of("cpi_se"),
           cpi_dk     = any_of("cpi_dk"),
           cpi_jp     = any_of("cpi_jp"),
           cpi_gb     = any_of("cpi_gb"),
           cpi_ch     = any_of("cpi_ch"))
  panel <- left_join(panel, fred_join, by = "date")
}

# Money supply
if (!is.null(money_supply) && nrow(money_supply) > 0) {
  ms_join <- money_supply |>
    mutate(date = floor_date(as.Date(date), "month")) |>
    pivot_wider(names_from = country, values_from = any_of(c("m2","m4_broad")),
                names_glue = "money_{.value}_{country}") |>
    select(date, any_of(c("money_m2_US", "money_m2_JP", "money_m4_broad_GB")))
  panel <- left_join(panel, ms_join, by = "date")
}

# =============================================================================
# Step 7: Key variable construction
# =============================================================================

log_info("=== Constructing key variables ===")

panel <- panel |>
  arrange(country, date) |>
  group_by(country) |>
  mutate(
    # CB assets as % of GDP
    cb_gdp_ratio  = case_when(
      !is.na(cb_assets) & !is.na(gdp_nominal_usd) & gdp_nominal_usd > 0 ~
        (cb_assets / (gdp_nominal_usd / 1e6)) * 100,   # both in millions USD
      TRUE ~ NA_real_
    ),

    # YoY change in CB/GDP ratio (12-month difference in percentage points)
    delta_cb_gdp  = cb_gdp_ratio - lag(cb_gdp_ratio, 12),

    # US CPI YoY (from index)
    cpi_yoy_us    = case_when(
      !is.na(us_cpi) ~ (us_cpi / lag(us_cpi, 12) - 1) * 100,
      TRUE           ~ NA_real_
    ),

    # Per-country CPI YoY (EA from ECB; US from FRED; others from OECD index via FRED)
    cpi_yoy       = case_when(
      country == "EA" ~ get("cpi_yoy_ea"),
      country == "US" ~ cpi_yoy_us,
      country == "NO" & !is.na(cpi_no) ~ (cpi_no / lag(cpi_no, 12) - 1) * 100,
      country == "SE" & !is.na(cpi_se) ~ (cpi_se / lag(cpi_se, 12) - 1) * 100,
      country == "DK" & !is.na(cpi_dk) ~ (cpi_dk / lag(cpi_dk, 12) - 1) * 100,
      country == "JP" & !is.na(cpi_jp) ~ (cpi_jp / lag(cpi_jp, 12) - 1) * 100,
      country == "GB" & !is.na(cpi_gb) ~ (cpi_gb / lag(cpi_gb, 12) - 1) * 100,
      country == "CH" & !is.na(cpi_ch) ~ (cpi_ch / lag(cpi_ch, 12) - 1) * 100,
      TRUE            ~ NA_real_
    ),

    # Per-country 10Y yield (EA from ECB; US from FRED; others from OECD via FRED)
    yield_10y     = case_when(
      country == "EA" ~ get("yield_10y_ea"),
      country == "US" ~ us_10y,
      country == "NO" ~ yield_no,
      country == "SE" ~ yield_se,
      country == "DK" ~ yield_dk,
      country == "JP" ~ yield_jp,
      country == "GB" ~ yield_gb,
      country == "CH" ~ yield_ch,
      TRUE            ~ NA_real_
    ),
    real_rate     = yield_10y - cpi_yoy,

    # GDP YoY growth
    gdp_yoy       = (gdp_nominal_usd / lag(gdp_nominal_usd, 12) - 1) * 100,
    log_gdp       = log(gdp_nominal_usd),

    # Money supply YoY (US only for now; extend when non-US series added)
    money_yoy     = case_when(
      country == "US" & "money_m2_US" %in% names(panel) ~
        (money_m2_US / lag(money_m2_US, 12) - 1) * 100,
      TRUE ~ NA_real_
    ),

    # Excess money growth = money YoY - GDP YoY
    excess_money_growth = money_yoy - gdp_yoy
  ) |>
  ungroup()

# =============================================================================
# Step 8: Chinn-Ito — expand annual to monthly (step function via na.locf)
# =============================================================================

log_info("=== Chinn-Ito annual -> monthly ===")

if (!is.null(chinnito) && nrow(chinnito) > 0) {
  # Create annual anchor at January 1 of each year
  chinnito_monthly_anchor <- chinnito |>
    mutate(date = make_date(year, 1, 1)) |>
    select(country, date, kaopen)

  # For each country, left join on year-month spine, then locf within group
  panel <- panel |>
    left_join(chinnito_monthly_anchor, by = c("country", "date")) |>
    group_by(country) |>
    mutate(kaopen = zoo::na.locf(kaopen, na.rm = FALSE)) |>
    ungroup()

  log_info("Chinn-Ito kaopen applied to panel")
} else {
  panel <- panel |> mutate(kaopen = NA_real_)
  log_warn("Chinn-Ito not available — kaopen = NA")
}

# =============================================================================
# Step 9: Factor variables and final column selection
# =============================================================================

log_info("=== Finalising panel ===")

panel <- panel |>
  mutate(
    country_f = factor(country, levels = COUNTRIES),
    year_f    = factor(year)
  ) |>
  select(
    date, country, year, month, quarter,
    cb_gdp_ratio, delta_cb_gdp, cb_assets, cb_assets_local, cb_assets_usd_millions = cb_assets, cb_source,
    cape, pe_ratio,
    cpi_yoy, yield_10y, real_rate,
    vix,
    gdp_nominal_usd, log_gdp, gdp_yoy,
    excess_money_growth,
    kaopen,
    country_f, year_f,
    everything()
  ) |>
  # Remove duplicated columns from everything()
  select(!any_of(c("cb_assets_usd_millions")))  # already have cb_assets

# Fix duplicate column issue from select(everything())
panel <- panel[, !duplicated(names(panel))]

log_info("Panel final: {nrow(panel)} rows, {ncol(panel)} columns, {n_distinct(panel$country)} countries")
log_info("Key columns: {paste(names(panel)[1:min(20,ncol(panel))], collapse=', ')}")

# =============================================================================
# Step 10: Quarterly aggregation
# =============================================================================

panel_quarterly <- panel |>
  filter(month %in% c(3, 6, 9, 12)) |>  # quarter-end months
  arrange(country, date)

log_info("Quarterly panel: {nrow(panel_quarterly)} rows")

# =============================================================================
# Step 11: Coverage summary
# =============================================================================

log_info("=== Computing coverage summary ===")

key_vars <- c("cb_gdp_ratio", "delta_cb_gdp", "cape", "pe_ratio",
              "cpi_yoy", "yield_10y", "real_rate", "vix",
              "gdp_nominal_usd", "kaopen", "excess_money_growth")

coverage_summary <- panel |>
  group_by(country) |>
  summarise(
    across(
      all_of(intersect(key_vars, names(panel))),
      ~ mean(!is.na(.x)) * 100,
      .names = "pct_{.col}"
    ),
    n_months      = n(),
    date_min      = min(date),
    date_max      = max(date),
    .groups       = "drop"
  )

print(coverage_summary)

# =============================================================================
# Save outputs
# =============================================================================

save_processed(panel,           "panel_monthly")
save_processed(panel_quarterly, "panel_quarterly")
save_processed(coverage_summary, "coverage_summary")

log_info("Script 04 complete.")
