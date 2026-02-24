# =============================================================================
# 08_event_study.R — Event Study: QE Announcements and Equity Markets
#
# Methodology (standard market model, Brown & Warner 1985):
#   Estimation window: [-220, -20] trading days relative to event date
#   Event window:      [-5,  +10] trading days
#   Market model:      R_i,t = alpha_i + beta_i * R_m,t + eps_i,t
#                      (market benchmark: S&P 500 log return)
#   Abnormal return:   AR_i,t = R_i,t - (alpha_hat + beta_hat * R_m,t)
#   CAR[t1, t2]:       sum of AR over [t1, t2]
#   t-statistic:       CAR / (sigma_hat * sqrt(n_days))
#
# CAR windows reported: [-1,0], [0,0], [0,+1], [-1,+1], [0,+5], [0,+10]
#
# Cross-country spillovers:
#   For each Tier-1 event, CAR[0,+5] is computed for ALL 8 equity markets,
#   not just the announcing country, to map international transmission.
#
# Outputs (output/figures/):
#   event_window_paths.{pdf,png}  — Avg cumulative return paths by event type
#   car_by_eventtype.{pdf,png}    — CAR summary bar chart (easing vs. tightening)
#   spillover_heatmap.{pdf,png}   — Cross-country spillover heatmap
#
# Outputs (output/logs/):
#   event_study_results.csv       — Full event-level CAR table
#   spillover_results.csv         — Event × country spillover CARs
#   event_study_report.txt        — Narrative text summary
#
# Requires: data/manual/qe_events.csv    (run: reviewed manually)
#           data/processed/equity_daily.rds (run: 07_collect_event_data.R)
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(lubridate)
  library(patchwork)
  library(scales)
  library(gt)
})

# =============================================================================
# Parameters
# =============================================================================

# Market model estimation window (trading days relative to event day 0)
EST_WIN_START <- -220
EST_WIN_END   <- -20

# Event window (trading days relative to event day 0)
EVT_WIN_START <- -5
EVT_WIN_END   <- +10

# Minimum estimation observations to fit a reliable market model
MIN_EST_OBS <- 80

# CAR windows to compute — names become column stems in the results table
# Format: stem = c(start, end) in relative trading days
CAR_WINDOWS <- list(
  m1_0   = c(-1,  0),   # day before + event day
  d0     = c( 0,  0),   # event day only
  d0_p1  = c( 0, +1),   # two-day [0, +1]
  m1_p1  = c(-1, +1),   # three-day [-1, +1]
  d0_p5  = c( 0, +5),   # one-week [0, +5]
  d0_p10 = c( 0,+10)    # two-week [0, +10]
)

# Human-readable labels for CAR window names (used in plots and reports)
CAR_LABELS <- c(
  m1_0   = "[-1, 0]",
  d0     = "[0, 0]",
  d0_p1  = "[0, +1]",
  m1_p1  = "[-1, +1]",
  d0_p5  = "[0, +5]",
  d0_p10 = "[0, +10]"
)

# =============================================================================
# Load data
# =============================================================================

log_info("=== Loading event study data ===")

events_path <- file.path(DIR_MANUAL, "qe_events.csv")
equity_path <- file.path(DIR_PROCESSED, "equity_daily.rds")

if (!file.exists(events_path)) stop("qe_events.csv not found in data/manual/")
if (!file.exists(equity_path)) stop("equity_daily.rds not found. Run 07_collect_event_data.R first.")

events <- read_csv(events_path, show_col_types = FALSE) |>
  mutate(date = as.Date(date))

equity_daily <- readRDS(equity_path)

market_ret <- equity_daily |>
  filter(country == "MARKET") |>
  select(date, r_mkt = log_return) |>
  arrange(date)

study_equity <- equity_daily |>
  filter(country != "MARKET") |>
  arrange(country, date)

log_info("Events loaded: {nrow(events)} total | Tier 1: {sum(events$tier == 1)}")
log_info("Countries with equity data: {paste(unique(study_equity$country), collapse = ', ')}")
log_info("Equity date range: {min(study_equity$date)} to {max(study_equity$date)}")

# =============================================================================
# Helper functions
# =============================================================================

# Fit OLS market model on estimation window; return NULL if insufficient data.
# Returns: list(alpha, beta, sigma, r_sq, n_est)
fit_market_model <- function(ctry_ret_df, mkt_ret_df) {
  df <- inner_join(ctry_ret_df, mkt_ret_df, by = "date") |>
    filter(!is.na(log_return), !is.na(r_mkt))

  if (nrow(df) < MIN_EST_OBS) return(NULL)

  fit <- lm(log_return ~ r_mkt, data = df)
  s   <- summary(fit)

  list(
    alpha  = unname(coef(fit)["(Intercept)"]),
    beta   = unname(coef(fit)["r_mkt"]),
    sigma  = sigma(fit),
    r_sq   = s$r.squared,
    n_est  = nrow(df)
  )
}

# Compute abnormal returns over event window given a fitted market model.
# Returns data frame with columns: date, log_return, r_mkt, expected, ar
compute_ar <- function(ctry_ret_df, mkt_ret_df, model) {
  inner_join(ctry_ret_df, mkt_ret_df, by = "date") |>
    filter(!is.na(log_return), !is.na(r_mkt)) |>
    arrange(date) |>
    mutate(
      expected = model$alpha + model$beta * r_mkt,
      ar       = log_return - expected
    )
}

# =============================================================================
# Main event study loop
# =============================================================================

log_info("=== Running market model event study ===")

results_summary <- list()
ar_daily_all    <- list()

for (i in seq_len(nrow(events))) {
  ev      <- events[i, ]
  ev_id   <- ev$event_id
  ev_date <- ev$date
  ev_ctry <- ev$country

  # Get sorted trading dates for this country
  ctry_df <- study_equity |>
    filter(country == ev_ctry) |>
    select(date, log_return) |>
    arrange(date)

  if (nrow(ctry_df) == 0) {
    log_warn("Event {ev_id} | {ev_ctry}: no equity data — skipping")
    next
  }

  trading_dates <- ctry_df$date

  # Locate event day: first trading day on or after ev_date (handles weekends/holidays)
  event_idx <- which(trading_dates >= ev_date)[1]
  if (is.na(event_idx)) {
    log_warn("Event {ev_id} | {ev_ctry} {ev_date}: beyond available data — skipping")
    next
  }

  # Map relative-day offsets to indices, clipping to data bounds
  to_idx <- function(offset) max(1L, min(length(trading_dates), event_idx + offset))

  est_start   <- to_idx(EST_WIN_START)
  est_end     <- to_idx(EST_WIN_END)
  evt_start   <- to_idx(EVT_WIN_START)
  evt_end     <- to_idx(EVT_WIN_END)

  # Check we have enough estimation data before the event
  if (est_end <= est_start) {
    log_warn("Event {ev_id} | {ev_ctry}: estimation window collapsed — skipping")
    next
  }

  est_range <- trading_dates[est_start:est_end]
  evt_range <- trading_dates[evt_start:evt_end]

  n_est_available <- length(est_range)
  if (n_est_available < MIN_EST_OBS) {
    log_warn(
      "Event {ev_id} | {ev_ctry} {ev_date}: only {n_est_available} estimation obs (min {MIN_EST_OBS}) — flagged as unreliable"
    )
  }

  # Subset returns to windows
  ctry_est  <- ctry_df |> filter(date %in% est_range)
  mkt_est   <- market_ret |> filter(date %in% est_range)
  ctry_evt  <- ctry_df |> filter(date %in% evt_range)
  mkt_evt   <- market_ret |> filter(date %in% evt_range)

  # Fit market model on estimation window
  model <- fit_market_model(ctry_est, mkt_est)
  if (is.null(model)) {
    log_warn("Event {ev_id} | {ev_ctry}: market model fit failed — skipping")
    next
  }

  log_info(
    "Event {ev_id} | {ev_ctry} {ev_date} | {ev$program_name} | beta={round(model$beta,3)} R²={round(model$r_sq,3)} n={model$n_est}"
  )

  # Compute abnormal returns over event window
  ar_df <- compute_ar(ctry_evt, mkt_evt, model)
  if (nrow(ar_df) == 0) {
    log_warn("Event {ev_id}: no event-window observations — skipping")
    next
  }

  # Assign relative day index (0 = first trading day on or after event date)
  ar_df <- ar_df |>
    mutate(rel_day = match(date, trading_dates) - event_idx)

  # Store daily AR data
  ar_daily_all[[ev_id]] <- ar_df |>
    mutate(
      event_id   = ev_id,
      country    = ev_ctry,
      event_type = ev$event_type,
      direction  = ev$direction
    )

  # Compute CARs and t-statistics for each window
  car_row <- tibble(
    event_id     = ev_id,
    date         = ev_date,
    country      = ev_ctry,
    central_bank = ev$central_bank,
    event_type   = ev$event_type,
    direction    = ev$direction,
    program_name = ev$program_name,
    surprise     = ev$surprise,
    tier         = ev$tier,
    alpha        = model$alpha,
    beta         = model$beta,
    sigma        = model$sigma,
    r_sq         = model$r_sq,
    n_est        = model$n_est,
    reliable     = (model$n_est >= MIN_EST_OBS)
  )

  for (win_stem in names(CAR_WINDOWS)) {
    w      <- CAR_WINDOWS[[win_stem]]
    win_ar <- ar_df |> filter(rel_day >= w[1], rel_day <= w[2])
    car_v  <- if (nrow(win_ar) > 0) sum(win_ar$ar, na.rm = TRUE) else NA_real_
    t_v    <- if (!is.na(car_v) && nrow(win_ar) > 0) {
      car_v / (model$sigma * sqrt(nrow(win_ar)))
    } else {
      NA_real_
    }
    car_row[[paste0("car_", win_stem)]] <- car_v
    car_row[[paste0("t_",   win_stem)]] <- t_v
  }

  results_summary[[ev_id]] <- car_row
}

# =============================================================================
# Compile and save results
# =============================================================================

results_df  <- bind_rows(results_summary)
ar_daily_df <- bind_rows(ar_daily_all)

log_info("Events with results: {nrow(results_df)} / {nrow(events)}")

write_csv(results_df, file.path(DIR_LOGS, "event_study_results.csv"))
log_info("Saved: event_study_results.csv")

# =============================================================================
# Cross-country spillover analysis (Tier-1 events only)
# =============================================================================

log_info("=== Cross-country spillover analysis ===")

tier1_events      <- events |> filter(tier == 1)
study_countries_v <- unique(study_equity$country)
spillover_rows    <- list()

for (i in seq_len(nrow(tier1_events))) {
  ev      <- tier1_events[i, ]
  ev_date <- ev$date

  for (ctry in study_countries_v) {
    ctry_df <- study_equity |>
      filter(country == ctry) |>
      select(date, log_return) |>
      arrange(date)

    if (nrow(ctry_df) == 0) next

    trading_dates <- ctry_df$date
    event_idx     <- which(trading_dates >= ev_date)[1]
    if (is.na(event_idx)) next

    to_idx <- function(offset) max(1L, min(length(trading_dates), event_idx + offset))

    est_start <- to_idx(EST_WIN_START)
    est_end   <- to_idx(EST_WIN_END)
    evt_start <- to_idx(0)
    evt_end   <- to_idx(5)

    if (est_end <= est_start) next

    est_range <- trading_dates[est_start:est_end]
    evt_range <- trading_dates[evt_start:evt_end]

    ctry_est  <- ctry_df |> filter(date %in% est_range)
    mkt_est   <- market_ret |> filter(date %in% est_range)
    ctry_evt  <- ctry_df |> filter(date %in% evt_range)
    mkt_evt   <- market_ret |> filter(date %in% evt_range)

    model <- fit_market_model(ctry_est, mkt_est)
    if (is.null(model)) next

    ar_df   <- compute_ar(ctry_evt, mkt_evt, model)
    car_0_5 <- if (nrow(ar_df) > 0) sum(ar_df$ar, na.rm = TRUE) else NA_real_

    spillover_rows[[paste(ev$event_id, ctry, sep = "_")]] <- tibble(
      event_id         = ev$event_id,
      event_date       = ev_date,
      event_country    = ev$country,
      affected_country = ctry,
      program_name     = ev$program_name,
      event_type       = ev$event_type,
      direction        = ev$direction,
      car_0_5          = car_0_5,
      is_domestic      = (ctry == ev$country)
    )
  }
}

spillover_df <- bind_rows(spillover_rows)
write_csv(spillover_df, file.path(DIR_LOGS, "spillover_results.csv"))
log_info("Spillover results: {nrow(spillover_df)} rows | saved spillover_results.csv")

# =============================================================================
# Figure 1: Average event-window cumulative return paths by event type
# =============================================================================

log_info("=== Generating figures ===")

event_type_labels <- c(
  initiation   = "Initiation",
  expansion    = "Expansion",
  tapering     = "Tapering",
  end          = "End of QE",
  shock_speech = "Signal/Speech",
  framework    = "Framework Change",
  restart      = "Restart"
)

# Average cumulative abnormal return path for easing events
avg_paths <- ar_daily_df |>
  filter(direction == 1, rel_day >= EVT_WIN_START, rel_day <= EVT_WIN_END) |>
  group_by(event_type, rel_day) |>
  summarise(
    mean_ar = mean(ar, na.rm = TRUE),
    se_ar   = sd(ar, na.rm = TRUE) / sqrt(n()),
    n       = n(),
    .groups = "drop"
  ) |>
  group_by(event_type) |>
  arrange(rel_day, .by_group = TRUE) |>
  mutate(
    cum_ar     = cumsum(mean_ar),
    cum_ar_pct = cum_ar * 100,
    se_cum_pct = se_ar * sqrt(row_number()) * 100   # propagated SE
  ) |>
  ungroup() |>
  mutate(event_label = recode(event_type, !!!event_type_labels))

p_paths <- avg_paths |>
  ggplot(aes(x = rel_day, y = cum_ar_pct, colour = event_label, fill = event_label)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.6, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = cum_ar_pct - 1.96 * se_cum_pct,
        ymax = cum_ar_pct + 1.96 * se_cum_pct),
    alpha = 0.12, colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(breaks = seq(EVT_WIN_START, EVT_WIN_END, by = 2)) +
  labs(
    title    = "Equity Market Response Around QE Announcements",
    subtitle = "Cumulative abnormal returns (market model); easing events only",
    x        = "Trading days relative to announcement (day 0)",
    y        = "Cumulative abnormal return (%)",
    colour   = "Event type",
    fill     = "Event type",
    caption  = paste0(
      "Market model estimated on [-220, -20] trading-day window with S&P 500 as benchmark. ",
      "Shaded bands: ±1.96 × propagated SE."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(DIR_FIGURES, "event_window_paths.pdf"), p_paths, width = 9, height = 6)
ggsave(file.path(DIR_FIGURES, "event_window_paths.png"), p_paths, width = 9, height = 6, dpi = 150)
log_info("Saved: event_window_paths.{{pdf,png}}")

# =============================================================================
# Figure 2: Average CARs by event type and direction (bar chart)
# =============================================================================

# Use [-1,+1] and [0,+5] as the two highlighted windows
car_plot_df <- results_df |>
  filter(!is.na(car_m1_p1)) |>
  mutate(
    direction_label = if_else(direction == 1, "Easing", "Tightening"),
    event_label     = recode(event_type, !!!event_type_labels)
  ) |>
  pivot_longer(
    cols      = c(car_m1_p1, car_d0_p5),
    names_to  = "window_stem",
    values_to = "car"
  ) |>
  mutate(
    window_label = recode(window_stem, car_m1_p1 = "CAR[-1,+1]", car_d0_p5 = "CAR[0,+5]"),
    car_pct      = car * 100
  ) |>
  group_by(event_label, direction_label, window_label) |>
  summarise(
    mean_car = mean(car_pct, na.rm = TRUE),
    se_car   = sd(car_pct, na.rm = TRUE) / sqrt(sum(!is.na(car_pct))),
    n        = n(),
    .groups  = "drop"
  )

p_car_bar <- car_plot_df |>
  ggplot(aes(x = event_label, y = mean_car, fill = direction_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = mean_car - 1.96 * se_car, ymax = mean_car + 1.96 * se_car),
    position = position_dodge(width = 0.75),
    width = 0.3
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  facet_wrap(~window_label, nrow = 1) +
  scale_fill_manual(values = c(Easing = "#2166ac", Tightening = "#d6604d")) +
  labs(
    title   = "Average Cumulative Abnormal Returns by QE Event Type",
    x       = NULL,
    y       = "Mean CAR (%)",
    fill    = "Direction",
    caption = "Error bars: ±1.96 SE across events. Number above bar = event count."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(DIR_FIGURES, "car_by_eventtype.pdf"), p_car_bar, width = 10, height = 6)
ggsave(file.path(DIR_FIGURES, "car_by_eventtype.png"), p_car_bar, width = 10, height = 6, dpi = 150)
log_info("Saved: car_by_eventtype.{{pdf,png}}")

# =============================================================================
# Figure 3: Cross-country spillover heatmap
# =============================================================================

if (nrow(spillover_df) > 0) {
  spill_avg <- spillover_df |>
    group_by(event_country, affected_country, direction) |>
    summarise(
      mean_car = mean(car_0_5, na.rm = TRUE) * 100,
      n_events = n(),
      .groups  = "drop"
    ) |>
    filter(direction == 1)   # easing events only

  country_order <- COUNTRIES

  p_spill <- spill_avg |>
    mutate(
      event_country    = factor(event_country,    levels = rev(country_order)),
      affected_country = factor(affected_country, levels = country_order)
    ) |>
    ggplot(aes(x = affected_country, y = event_country, fill = mean_car)) +
    geom_tile(colour = "white", linewidth = 0.7) +
    geom_text(
      aes(label = sprintf("%+.2f%%", mean_car)),
      size = 3, colour = "grey10"
    ) +
    scale_fill_gradient2(
      low      = "#d6604d",
      mid      = "#f7f7f7",
      high     = "#2166ac",
      midpoint = 0,
      name     = "CAR[0,+5]"
    ) +
    labs(
      title    = "Cross-Country Equity Spillovers from QE Announcements",
      subtitle = "Average CAR[0,+5] in affected equity market; row = announcing central bank",
      x        = "Affected equity market",
      y        = "Central bank announcing QE",
      caption  = "Tier-1 easing events only. Market model with S&P 500 benchmark."
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "right")

  ggsave(file.path(DIR_FIGURES, "spillover_heatmap.pdf"), p_spill, width = 8, height = 6)
  ggsave(file.path(DIR_FIGURES, "spillover_heatmap.png"), p_spill, width = 8, height = 6, dpi = 150)
  log_info("Saved: spillover_heatmap.{{pdf,png}}")
}

# =============================================================================
# Text report
# =============================================================================

report_lines <- c(
  strrep("=", 72),
  "EVENT STUDY REPORT — QE Announcements and Equity Markets",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  strrep("=", 72),
  "",
  "METHODOLOGY",
  "  Market model: R_i,t = alpha + beta * R_SP500,t + eps_i,t",
  glue("  Estimation window: [{EST_WIN_START}, {EST_WIN_END}] trading days"),
  glue("  Event window:      [{EVT_WIN_START}, {EVT_WIN_END}] trading days"),
  glue("  Min estimation obs: {MIN_EST_OBS}"),
  "",
  "SAMPLE",
  glue("  Total events in database:  {nrow(events)}"),
  glue("  Events with valid results: {nrow(results_df)}"),
  glue("  Easing events (direction=+1): {sum(results_df$direction == 1, na.rm=TRUE)}"),
  glue("  Tightening events (direction=-1): {sum(results_df$direction == -1, na.rm=TRUE)}"),
  ""
)

# Average CARs by event type for easing events
report_lines <- c(report_lines, strrep("-", 72),
                  "AVERAGE CARs BY EVENT TYPE (easing events)", strrep("-", 72), "")

for (etype in sort(unique(results_df$event_type[results_df$direction == 1]))) {
  sub <- results_df |> filter(event_type == etype, direction == 1)
  report_lines <- c(
    report_lines,
    glue("  {etype}  (n = {nrow(sub)})"),
    sprintf(
      "    CAR[-1,+1] = %+.3f%%  |  CAR[0,+5] = %+.3f%%  |  CAR[0,+10] = %+.3f%%",
      mean(sub$car_m1_p1  * 100, na.rm = TRUE),
      mean(sub$car_d0_p5  * 100, na.rm = TRUE),
      mean(sub$car_d0_p10 * 100, na.rm = TRUE)
    ),
    ""
  )
}

# Individual event table
report_lines <- c(
  report_lines,
  strrep("-", 90),
  "INDIVIDUAL EVENT RESULTS",
  strrep("-", 90),
  sprintf(
    "%-4s  %-10s  %-4s  %-14s  %-14s  %9s  %8s  %9s  %s",
    "ID", "Date", "Ctry", "Program", "Type",
    "CAR[-1+1]", "CAR[0+5]", "CAR[0+10]", "t[-1+1]"
  ),
  strrep("-", 90)
)

for (i in seq_len(nrow(results_df))) {
  r <- results_df[i, ]
  sig <- dplyr::case_when(
    abs(r$t_m1_p1) >= 2.576 ~ "***",
    abs(r$t_m1_p1) >= 1.960 ~ "**",
    abs(r$t_m1_p1) >= 1.645 ~ "*",
    TRUE                     ~ ""
  )
  report_lines <- c(
    report_lines,
    sprintf(
      "%-4s  %-10s  %-4s  %-14s  %-14s  %+8.3f%%  %+7.3f%%  %+8.3f%%  %+6.2f%s",
      r$event_id, format(r$date), r$country, r$program_name, r$event_type,
      coalesce(r$car_m1_p1  * 100, NA_real_),
      coalesce(r$car_d0_p5  * 100, NA_real_),
      coalesce(r$car_d0_p10 * 100, NA_real_),
      coalesce(r$t_m1_p1,         NA_real_),
      sig
    )
  )
}

report_lines <- c(
  report_lines, "",
  "* p<0.10  ** p<0.05  *** p<0.01 (two-tailed, sigma from estimation window)",
  "",
  strrep("-", 72),
  "KNOWN LIMITATIONS",
  strrep("-", 72),
  "1. US benchmark circularity: S&P 500 used as market model benchmark for US",
  "   equity. US CAR estimates are mechanically near zero. Use an ex-US index",
  "   (e.g. MSCI EAFE / ticker EFA) as the market benchmark for US regressions.",
  "",
  "2. Event clustering: Some events fall within the estimation windows of others",
  "   (e.g. ECB March and June 2020). Market model alphas/betas may be biased",
  "   for events with contaminated estimation windows.",
  "",
  "3. t-statistics use time-series sigma from the estimation window and do not",
  "   adjust for event-induced variance increases (Boehmer et al. 1991). The",
  "   simple t-test is conservative when QE announcements raise volatility.",
  ""
)

report_path <- file.path(DIR_LOGS, "event_study_report.txt")
writeLines(report_lines, report_path)
log_info("Saved: event_study_report.txt")

# =============================================================================
# Summary log
# =============================================================================

log_info("=== 08_event_study.R complete ===")
log_info("Outputs:")
log_info("  {DIR_FIGURES}/event_window_paths.{{pdf,png}}")
log_info("  {DIR_FIGURES}/car_by_eventtype.{{pdf,png}}")
log_info("  {DIR_FIGURES}/spillover_heatmap.{{pdf,png}}")
log_info("  {DIR_LOGS}/event_study_results.csv")
log_info("  {DIR_LOGS}/spillover_results.csv")
log_info("  {DIR_LOGS}/event_study_report.txt")
