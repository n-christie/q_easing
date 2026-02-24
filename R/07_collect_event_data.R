# =============================================================================
# 07_collect_event_data.R — Daily Equity Index Data for Event Study
# Source: Stooq (stooq.com) — free, no authentication required
#
# Yahoo Finance has blocked automated access since ~2023; this script fetches
# directly from Stooq's CSV download endpoint using httr.
#
# Stooq URL format:
#   https://stooq.com/q/d/l/?s={ticker}&d1={YYYYMMDD}&d2={YYYYMMDD}&i=d
#
# Outputs:
#   data/raw/equity_daily/   — per-ticker raw CSV files
#   data/processed/equity_daily.{rds,csv}      — log returns, tidy format
#   data/processed/equity_daily_coverage.rds   — coverage summary
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(httr)
  library(lubridate)
})

# =============================================================================
# Equity index tickers (Stooq)
# =============================================================================
#
# Confirmed working on Stooq with history from 2007:
#   ^spx    — S&P 500 Composite (US)
#   ^dax    — DAX (Germany; used as Euro Area proxy — largest eurozone economy)
#   ^nkx    — Nikkei 225 (JP)
#   ^ukx    — FTSE 100 (GB)
#   ^omxs   — OMX Stockholm 30 (SE)
#   ^oseax  — Oslo Stock Exchange All Share (NO)
#   ^smi    — Swiss Market Index (CH)
#
# DK: OMX Copenhagen is not available on Stooq. DAX (^dax) is used as a proxy
# because the DKK is pegged to EUR and Danish equities closely track eurozone
# markets. DK has no QE events in the database, so it appears only as a
# spillover recipient — the proxy is adequate for that purpose.
#
# MARKET benchmark: ^spx (S&P 500) used as global market factor in market model.
# NOTE on US circularity: the domestic US equity index and the market benchmark
# are the same series. US CARs will be near zero by construction. The spillover
# analysis (US QE → non-US markets) is unaffected.

EQUITY_TICKERS <- c(
  US     = "^spx",
  EA     = "^dax",      # DAX as Euro Area proxy
  JP     = "^nkx",      # Nikkei 225
  GB     = "^ukx",      # FTSE 100
  SE     = "^omxs",     # OMX Stockholm 30
  NO     = "^oseax",    # Oslo SE All Share
  DK     = "^dax",      # DAX proxy (DK pegged to EUR; no OMX Copenhagen on Stooq)
  CH     = "^smi"       # Swiss Market Index
)

MARKET_TICKER  <- "^spx"
MARKET_COUNTRY <- "MARKET"

# =============================================================================
# Date range
# =============================================================================

events_path <- file.path(DIR_MANUAL, "qe_events.csv")

if (!file.exists(events_path)) {
  log_error("qe_events.csv not found at {events_path}")
  stop("Place qe_events.csv in data/manual/ before running this script.")
}

events <- read_csv(events_path, show_col_types = FALSE) |>
  mutate(date = as.Date(date))

# 365 calendar days before earliest event covers the [-220, -20] estimation window
FETCH_START <- min(events$date) - 365
FETCH_END   <- max(events$date) + 60

log_info("Events: {nrow(events)} across {n_distinct(events$country)} countries")
log_info("Event date range: {min(events$date)} to {max(events$date)}")
log_info("Fetching daily prices: {FETCH_START} to {FETCH_END}")

# =============================================================================
# Stooq download helper
# =============================================================================

dir_equity <- file.path(DIR_RAW, "equity_daily")
fs::dir_create(dir_equity)

fetch_stooq <- function(ticker, from_date, to_date) {
  ticker_enc <- URLencode(tolower(ticker), reserved = TRUE)
  d1 <- format(from_date, "%Y%m%d")
  d2 <- format(to_date,   "%Y%m%d")
  url <- glue("https://stooq.com/q/d/l/?s={ticker_enc}&d1={d1}&d2={d2}&i=d")

  resp <- safe_fetch(
    httr::GET(url, httr::timeout(30)),
    source_name = "Stooq",
    series_id   = ticker
  )

  if (is.null(resp) || httr::http_error(resp)) {
    log_warn("  {ticker}: HTTP error or NULL response")
    return(NULL)
  }

  txt <- httr::content(resp, "text", encoding = "UTF-8")

  df <- tryCatch(
    read_csv(I(txt), show_col_types = FALSE, name_repair = "minimal"),
    error = function(e) {
      log_error("  {ticker}: CSV parse failed — {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0) {
    log_warn("  {ticker}: empty response (ticker may not exist on Stooq)")
    return(NULL)
  }

  # Stooq columns: Date, Open, High, Low, Close, Volume (Volume may be absent)
  if (!"Date"  %in% names(df) || !"Close" %in% names(df)) {
    log_warn("  {ticker}: unexpected columns — {paste(names(df), collapse=', ')}")
    return(NULL)
  }

  df |>
    transmute(
      date   = as.Date(Date),
      close  = suppressWarnings(as.numeric(Close))
    ) |>
    filter(!is.na(date), !is.na(close)) |>
    arrange(date)
}

# =============================================================================
# Download all tickers
# =============================================================================

unique_tickers <- unique(c(unname(EQUITY_TICKERS), MARKET_TICKER))

log_info("=== Downloading {length(unique_tickers)} equity series from Stooq ===")
log_info("Tickers: {paste(unique_tickers, collapse = ', ')}")

raw_list <- list()
for (tkr in unique_tickers) {
  log_info("Fetching: {tkr}")
  raw_list[[tkr]] <- fetch_stooq(tkr, FETCH_START, FETCH_END)
  Sys.sleep(0.5)   # be polite to Stooq
}

# Save raw per-ticker RDS
for (tkr in names(raw_list)) {
  if (!is.null(raw_list[[tkr]])) {
    stem <- gsub("[^a-zA-Z0-9]", "_", tkr)
    save_raw(raw_list[[tkr]], "equity_daily", paste0("raw_", stem))
  }
}

# =============================================================================
# Build tidy returns dataset
# =============================================================================

# Expand ticker lookup to include MARKET label
ticker_lookup <- bind_rows(
  tibble(ticker = unname(EQUITY_TICKERS), country = names(EQUITY_TICKERS)),
  tibble(ticker = MARKET_TICKER,          country = MARKET_COUNTRY)
)

# One row per country × date — for countries sharing a ticker (DK and EA both
# use ^dax), the same price series is duplicated under both country labels
equity_daily <- ticker_lookup |>
  group_by(country) |>
  reframe({
    tkr  <- ticker
    df   <- raw_list[[tkr]]
    if (is.null(df)) {
      tibble(date = as.Date(character()), price = numeric(), log_return = numeric(), ticker_used = character())
    } else {
      df |>
        arrange(date) |>
        mutate(
          log_return  = log(close / lag(close)),
          ticker_used = tkr
        ) |>
        filter(!is.na(log_return)) |>
        transmute(date, price = close, log_return, ticker_used)
    }
  })

# =============================================================================
# Coverage report
# =============================================================================

coverage <- equity_daily |>
  group_by(country, ticker_used) |>
  summarise(
    n_obs = n(),
    start = min(date),
    end   = max(date),
    .groups = "drop"
  )

log_info("=== Daily equity coverage ===")
for (i in seq_len(nrow(coverage))) {
  r <- coverage[i, ]
  log_info("  {r$country} ({r$ticker_used}): {r$n_obs} obs | {r$start} to {r$end}")
}

missing <- setdiff(c(names(EQUITY_TICKERS), MARKET_COUNTRY), coverage$country)
if (length(missing) > 0) {
  log_warn("No data for: {paste(missing, collapse=', ')}")
  log_warn("Events for these countries will be skipped in 08_event_study.R")
}

# Save
save_processed(equity_daily, "equity_daily")
saveRDS(coverage, file.path(DIR_PROCESSED, "equity_daily_coverage.rds"))

log_info("=== 07_collect_event_data.R complete ===")
log_info("Next: run R/08_event_study.R")
