# =============================================================================
# 09_collect_yield_data.R — Daily 10Y Government Bond Yields
#
# Fetches daily 10-year government bond yields from four central bank /
# treasury data portals, covering the countries for which the bond yield
# event study in 10_yield_event_study.R can be run.
#
# Sources and coverage:
#   US  — US Department of the Treasury (daily yield curve CSV, monthly files)
#   EA  — ECB Statistical Data Warehouse, AAA euro area yield curve (YC dataflow)
#   GB  — Bank of England IADB, series IUDMNPY (10Y nominal par yield)
#   SE  — Sveriges Riksbank REST API, series SEGVB10YC
#   JP  — Unavailable: Japan MoF historical files are not accessible; BoJ API
#          returned 404. JP yield events are excluded from the yield study.
#   CH  — Unavailable: SNB API connection reset. CH yield events excluded.
#   NO, DK — No QE events; excluded from yield study.
#
# Outputs:
#   data/raw/yield_daily/           — per-source raw files
#   data/processed/yield_daily.{rds,csv}  — tidy daily yield levels + changes
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(httr)
  library(lubridate)
  library(jsonlite)
})

# =============================================================================
# Date range (same as equity study)
# =============================================================================

events_path <- file.path(DIR_MANUAL, "qe_events.csv")
if (!file.exists(events_path)) stop("qe_events.csv not found in data/manual/")

events     <- read_csv(events_path, show_col_types = FALSE) |> mutate(date = as.Date(date))
FETCH_START <- min(events$date) - 365
FETCH_END   <- max(events$date) + 60

log_info("Fetching daily 10Y yields: {FETCH_START} to {FETCH_END}")

dir_yield <- file.path(DIR_RAW, "yield_daily")
fs::dir_create(dir_yield)

# =============================================================================
# 1. United States — US Treasury yield curve CSV (monthly files)
# =============================================================================
# URL: https://home.treasury.gov/resource-center/data-chart-center/interest-rates/
#      daily-treasury-rates.csv/all/{YYYYMM}?...&type=daily_treasury_yield_curve
# Returns: CSV with columns Date, "1 Mo", "3 Mo", ... "10 Yr", ...
# =============================================================================

log_info("=== 1. US Treasury 10Y yield ===")

ust_months <- seq(
  floor_date(FETCH_START, "month"),
  floor_date(FETCH_END,   "month"),
  by = "month"
)
log_info("Looping {length(ust_months)} months from {min(ust_months)} to {max(ust_months)}")

ust_list <- vector("list", length(ust_months))

for (i in seq_along(ust_months)) {
  ym_str <- format(ust_months[[i]], "%Y%m")
  url    <- paste0(
    "https://home.treasury.gov/resource-center/data-chart-center/",
    "interest-rates/daily-treasury-rates.csv/all/", ym_str,
    "?field_tdr_date_value=", ym_str,
    "&type=daily_treasury_yield_curve&page&_format=csv"
  )

  resp <- safe_fetch(httr::GET(url, httr::timeout(20)), "UST", ym_str)
  if (is.null(resp) || httr::http_error(resp)) next

  txt <- httr::content(resp, "text", encoding = "UTF-8")
  if (nchar(txt) == 0) next

  df <- tryCatch(
    read_csv(I(txt), show_col_types = FALSE, name_repair = "minimal"),
    error = function(e) NULL
  )

  if (!is.null(df) && "10 Yr" %in% names(df)) {
    ust_list[[i]] <- df |>
      transmute(
        date      = as.Date(Date, "%m/%d/%Y"),
        yield_10y = suppressWarnings(as.numeric(`10 Yr`))
      ) |>
      filter(!is.na(date), !is.na(yield_10y))
  }

  if (i %% 24 == 0) log_info("  UST progress: {i}/{length(ust_months)} months")
  Sys.sleep(0.4)
}

ust_df <- bind_rows(ust_list) |> arrange(date)
log_info("UST: {nrow(ust_df)} obs | {min(ust_df$date)} to {max(ust_df$date)}")
saveRDS(ust_df, file.path(dir_yield, "ust_10y_raw.rds"))

# =============================================================================
# 2. Euro Area — ECB Statistical Data Warehouse, AAA yield curve 10Y spot rate
# =============================================================================
# Series: YC / B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y
# Units:  % p.a. (continuous compounding)
# =============================================================================

log_info("=== 2. ECB SDW AAA euro area 10Y yield ===")

ecb_url <- paste0(
  "https://data-api.ecb.europa.eu/service/data/",
  "YC/B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y",
  "?format=csvdata",
  "&startPeriod=", format(FETCH_START, "%Y-%m-%d"),
  "&endPeriod=",   format(FETCH_END,   "%Y-%m-%d")
)

ecb_resp <- safe_fetch(httr::GET(ecb_url, httr::timeout(60)), "ECB_SDW", "YC_10Y")

ecb_df <- NULL
if (!is.null(ecb_resp) && !httr::http_error(ecb_resp)) {
  txt <- httr::content(ecb_resp, "text", encoding = "UTF-8")
  raw_ecb <- tryCatch(
    read_csv(I(txt), show_col_types = FALSE, name_repair = "minimal"),
    error = function(e) NULL
  )
  if (!is.null(raw_ecb) && "TIME_PERIOD" %in% names(raw_ecb) && "OBS_VALUE" %in% names(raw_ecb)) {
    ecb_df <- raw_ecb |>
      transmute(
        date      = as.Date(TIME_PERIOD),
        yield_10y = suppressWarnings(as.numeric(OBS_VALUE))
      ) |>
      filter(!is.na(date), !is.na(yield_10y)) |>
      arrange(date)
    log_info("ECB: {nrow(ecb_df)} obs | {min(ecb_df$date)} to {max(ecb_df$date)}")
    saveRDS(ecb_df, file.path(dir_yield, "ecb_10y_raw.rds"))
  }
}

# =============================================================================
# 3. United Kingdom — Bank of England IADB, series IUDMNPY
# =============================================================================
# URL: https://www.bankofengland.co.uk/boeapps/iadb/fromshowcolumns.asp?csv.x=yes
#      &Datefrom=DD/Mon/YYYY&Dateto=DD/Mon/YYYY&SeriesCodes=IUDMNPY&CSVF=TT
# =============================================================================

log_info("=== 3. Bank of England 10Y gilt yield ===")

boe_url <- paste0(
  "https://www.bankofengland.co.uk/boeapps/iadb/fromshowcolumns.asp",
  "?csv.x=yes",
  "&Datefrom=", format(FETCH_START, "%d/%b/%Y"),
  "&Dateto=",   format(FETCH_END,   "%d/%b/%Y"),
  "&SeriesCodes=IUDMNPY&CSVF=TT&UsingCodes=Y"
)

boe_resp <- safe_fetch(httr::GET(boe_url, httr::timeout(60)), "BoE_IADB", "IUDMNPY")

boe_df <- NULL
if (!is.null(boe_resp) && !httr::http_error(boe_resp)) {
  txt     <- httr::content(boe_resp, "text", encoding = "UTF-8")
  lines   <- strsplit(txt, "\n")[[1]]
  # BoE CSV has 4 header rows before data
  data_lines <- lines[grepl("^[0-9]", lines)]
  if (length(data_lines) > 0) {
    boe_df <- tibble(raw = data_lines) |>
      separate(raw, into = c("date_str", "yield_10y"), sep = ",", extra = "drop") |>
      mutate(
        date      = as.Date(trimws(date_str), "%d %b %Y"),
        yield_10y = suppressWarnings(as.numeric(trimws(yield_10y)))
      ) |>
      filter(!is.na(date), !is.na(yield_10y)) |>
      arrange(date)
    log_info("BoE: {nrow(boe_df)} obs | {min(boe_df$date)} to {max(boe_df$date)}")
    saveRDS(boe_df, file.path(dir_yield, "boe_10y_raw.rds"))
  }
}

# =============================================================================
# 4. Sweden — Riksbank REST API, series SEGVB10YC
# =============================================================================
# URL: https://api.riksbank.se/swea/v1/Observations/{series}/{from}/{to}
# Returns: JSON array of [{date, value}]
# =============================================================================

log_info("=== 4. Riksbank 10Y Swedish government bond yield ===")

rb_url <- paste0(
  "https://api.riksbank.se/swea/v1/Observations/SEGVB10YC/",
  format(FETCH_START, "%Y-%m-%d"), "/",
  format(FETCH_END,   "%Y-%m-%d")
)

rb_resp <- safe_fetch(httr::GET(rb_url, httr::timeout(60)), "Riksbank", "SEGVB10YC")

rb_df <- NULL
if (!is.null(rb_resp) && !httr::http_error(rb_resp)) {
  txt <- httr::content(rb_resp, "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(txt),
    error = function(e) NULL
  )
  if (!is.null(parsed) && is.data.frame(parsed) && "date" %in% names(parsed)) {
    rb_df <- parsed |>
      transmute(
        date      = as.Date(date),
        yield_10y = suppressWarnings(as.numeric(value))
      ) |>
      filter(!is.na(date), !is.na(yield_10y)) |>
      arrange(date)
    log_info("Riksbank: {nrow(rb_df)} obs | {min(rb_df$date)} to {max(rb_df$date)}")
    saveRDS(rb_df, file.path(dir_yield, "riksbank_10y_raw.rds"))
  }
}

# =============================================================================
# 5. Combine into tidy panel
# =============================================================================

log_info("=== 5. Building yield panel ===")

yield_sources <- list(
  US = ust_df,
  EA = ecb_df,
  GB = boe_df,
  SE = rb_df
)

yield_daily <- bind_rows(
  lapply(names(yield_sources), function(ctry) {
    df <- yield_sources[[ctry]]
    if (is.null(df) || nrow(df) == 0) {
      log_warn("  {ctry}: no yield data available")
      return(NULL)
    }
    df |>
      arrange(date) |>
      mutate(
        country      = ctry,
        delta_yield  = yield_10y - lag(yield_10y)  # daily yield change (pct pts)
      ) |>
      filter(!is.na(delta_yield)) |>
      select(country, date, yield_10y, delta_yield)
  })
)

# =============================================================================
# Coverage report
# =============================================================================

coverage <- yield_daily |>
  group_by(country) |>
  summarise(
    n_obs = n(),
    start = min(date),
    end   = max(date),
    .groups = "drop"
  )

log_info("=== Daily yield coverage ===")
for (i in seq_len(nrow(coverage))) {
  r <- coverage[i, ]
  log_info("  {r$country}: {r$n_obs} obs | {r$start} to {r$end}")
}

countries_missing <- setdiff(c("US","EA","GB","JP","SE","CH"), coverage$country)
if (length(countries_missing) > 0) {
  log_warn("No yield data for: {paste(countries_missing, collapse=', ')}")
  log_warn("Events for these countries will be excluded from 10_yield_event_study.R")
}

save_processed(yield_daily, "yield_daily")

log_info("=== 09_collect_yield_data.R complete ===")
log_info("Next: run R/10_yield_event_study.R")
