# =============================================================================
# 10_yield_event_study.R — Bond Yield Event Study: QE Announcements
#
# Methodology (mean-adjusted model, standard for yield event studies):
#   Estimation window:  [-220, -20] trading days relative to event date
#   Event window:       [-5,  +10] trading days
#   Mean-adjusted AR:   AR_i,t = Δy_i,t - mean(Δy_i during estimation window)
#   CAYC[t1, t2]:       sum of AR_i over [t1, t2]   (in percentage points)
#   t-statistic:        CAYC / (sigma_hat * sqrt(n_days))
#
# No market factor is used (unlike the equity study). This avoids:
#   (a) circularity when the announcing country's yield is also the benchmark
#   (b) removing genuine global rate movements driven by the QE policy itself
#
# Key sign convention:
#   CAYC < 0 → yields FELL (bond prices rose) — expected for easing events
#   CAYC > 0 → yields ROSE  — expected for tapering/tightening events
#
# Countries with daily yield data: US, EA, GB, SE
# Countries unavailable (excluded from yield study): JP, CH, NO, DK
#
# Additional output: joint equity + yield comparison figure, using CARs from
# 08_event_study.R alongside CAYCs from this script for matching events.
#
# Outputs (output/figures/):
#   yield_window_paths.{pdf,png}      — Avg cumulative yield paths by event type
#   yield_car_by_eventtype.{pdf,png}  — CAYC bar chart by event type + direction
#   equity_vs_yield.{pdf,png}         — Scatter: equity CAR vs bond CAYC by event
#
# Outputs (output/logs/):
#   yield_study_results.csv           — Full event-level CAYC table
#   yield_study_report.txt            — Narrative summary
#
# Requires:
#   data/manual/qe_events.csv         — event database
#   data/processed/yield_daily.rds    — from 09_collect_yield_data.R
#   data/processed/equity_daily.rds   — (optional) for joint figure
#   output/logs/event_study_results.csv — (optional) for joint figure
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(lubridate)
  library(patchwork)
  library(scales)
})

# =============================================================================
# Parameters (match equity study for comparability)
# =============================================================================

EST_WIN_START  <- -220
EST_WIN_END    <- -20
EVT_WIN_START  <- -5
EVT_WIN_END    <- +10
MIN_EST_OBS    <-  80

CAR_WINDOWS <- list(
  m1_0   = c(-1,  0),
  d0     = c( 0,  0),
  d0_p1  = c( 0, +1),
  m1_p1  = c(-1, +1),
  d0_p5  = c( 0, +5),
  d0_p10 = c( 0,+10)
)

# =============================================================================
# Load data
# =============================================================================

log_info("=== Loading yield event study data ===")

events_path <- file.path(DIR_MANUAL,    "qe_events.csv")
yield_path  <- file.path(DIR_PROCESSED, "yield_daily.rds")

if (!file.exists(events_path)) stop("qe_events.csv not found in data/manual/")
if (!file.exists(yield_path))  stop("yield_daily.rds not found. Run 09_collect_yield_data.R first.")

events      <- read_csv(events_path, show_col_types = FALSE) |> mutate(date = as.Date(date))
yield_daily <- readRDS(yield_path)

available_countries <- unique(yield_daily$country)
events_covered      <- events |> filter(country %in% available_countries)
events_excluded     <- events |> filter(!country %in% available_countries)

log_info("Yield data available for: {paste(available_countries, collapse=', ')}")
log_info("Events covered: {nrow(events_covered)} | Excluded (no yield data): {nrow(events_excluded)}")
if (nrow(events_excluded) > 0) {
  log_warn("Excluded: {paste(paste(events_excluded$country, events_excluded$program_name, sep='-'), collapse=', ')}")
}

# =============================================================================
# Helper functions
# =============================================================================

# Mean-adjusted model: returns mean and sigma from estimation window
fit_mean_model <- function(delta_yield_vec) {
  if (length(delta_yield_vec) < MIN_EST_OBS) return(NULL)
  list(
    mu    = mean(delta_yield_vec, na.rm = TRUE),
    sigma = sd(delta_yield_vec,   na.rm = TRUE),
    n_est = sum(!is.na(delta_yield_vec))
  )
}

# =============================================================================
# Main event study loop
# =============================================================================

log_info("=== Running mean-adjusted yield event study ===")

results_summary <- list()
ar_daily_all    <- list()

for (i in seq_len(nrow(events_covered))) {
  ev      <- events_covered[i, ]
  ev_id   <- ev$event_id
  ev_date <- ev$date
  ev_ctry <- ev$country

  ctry_df <- yield_daily |>
    filter(country == ev_ctry) |>
    select(date, delta_yield) |>
    arrange(date)

  if (nrow(ctry_df) == 0) next

  trading_dates <- ctry_df$date
  event_idx     <- which(trading_dates >= ev_date)[1]
  if (is.na(event_idx)) next

  to_idx <- function(off) max(1L, min(length(trading_dates), event_idx + off))

  est_range <- trading_dates[to_idx(EST_WIN_START):to_idx(EST_WIN_END)]
  evt_range <- trading_dates[to_idx(EVT_WIN_START):to_idx(EVT_WIN_END)]

  if (length(est_range) < MIN_EST_OBS) {
    log_warn("Event {ev_id} | {ev_ctry}: only {length(est_range)} est obs — flagged")
  }

  est_deltas <- ctry_df |> filter(date %in% est_range) |> pull(delta_yield)
  model      <- fit_mean_model(est_deltas)
  if (is.null(model)) {
    log_warn("Event {ev_id} | {ev_ctry}: model fit failed — skipping")
    next
  }

  evt_df <- ctry_df |>
    filter(date %in% evt_range) |>
    mutate(
      ar      = delta_yield - model$mu,
      rel_day = match(date, trading_dates) - event_idx
    )

  if (nrow(evt_df) == 0) next

  log_info("Event {ev_id} | {ev_ctry} {ev_date} | {ev$program_name} | mu={round(model$mu*100,2)}bp sigma={round(model$sigma*100,2)}bp n={model$n_est}")

  ar_daily_all[[ev_id]] <- evt_df |>
    mutate(event_id = ev_id, country = ev_ctry, event_type = ev$event_type, direction = ev$direction)

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
    est_mu_bp    = model$mu    * 100,   # mean daily yield chg in basis points
    est_sigma_bp = model$sigma * 100,
    n_est        = model$n_est,
    reliable     = (model$n_est >= MIN_EST_OBS)
  )

  for (win_stem in names(CAR_WINDOWS)) {
    w      <- CAR_WINDOWS[[win_stem]]
    win_ar <- evt_df |> filter(rel_day >= w[1], rel_day <= w[2])
    cayc_v <- if (nrow(win_ar) > 0) sum(win_ar$ar, na.rm = TRUE) * 100 else NA_real_  # in bp
    t_v    <- if (!is.na(cayc_v) && nrow(win_ar) > 0) {
      cayc_v / (model$sigma * 100 * sqrt(nrow(win_ar)))
    } else NA_real_
    car_row[[paste0("cayc_", win_stem)]] <- cayc_v
    car_row[[paste0("t_",    win_stem)]] <- t_v
  }

  results_summary[[ev_id]] <- car_row
}

# =============================================================================
# Compile and save
# =============================================================================

results_df  <- bind_rows(results_summary)
ar_daily_df <- bind_rows(ar_daily_all)

log_info("Events with results: {nrow(results_df)} / {nrow(events_covered)}")
write_csv(results_df, file.path(DIR_LOGS, "yield_study_results.csv"))
log_info("Saved: yield_study_results.csv")

# =============================================================================
# Figure 1: Average cumulative yield change paths by event type
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

avg_paths <- ar_daily_df |>
  filter(direction == 1, rel_day >= EVT_WIN_START, rel_day <= EVT_WIN_END) |>
  group_by(event_type, rel_day) |>
  summarise(
    mean_ar = mean(ar * 100, na.rm = TRUE),
    se_ar   = sd(ar * 100,   na.rm = TRUE) / sqrt(n()),
    n       = n(),
    .groups = "drop"
  ) |>
  group_by(event_type) |>
  arrange(rel_day, .by_group = TRUE) |>
  mutate(
    cum_ar     = cumsum(mean_ar),
    se_cum     = se_ar * sqrt(row_number())
  ) |>
  ungroup() |>
  mutate(event_label = recode(event_type, !!!event_type_labels))

p_yield_paths <- avg_paths |>
  ggplot(aes(x = rel_day, y = cum_ar, colour = event_label, fill = event_label)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.6, linetype = "dashed") +
  geom_ribbon(
    aes(ymin = cum_ar - 1.96 * se_cum, ymax = cum_ar + 1.96 * se_cum),
    alpha = 0.12, colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(breaks = seq(EVT_WIN_START, EVT_WIN_END, by = 2)) +
  labs(
    title    = "Bond Yield Response Around QE Announcements",
    subtitle = "Cumulative abnormal yield change (mean-adjusted model, bp); easing events only",
    x        = "Trading days relative to announcement (day 0)",
    y        = "Cumulative abnormal yield change (basis points)",
    colour   = "Event type",
    fill     = "Event type",
    caption  = paste0(
      "Mean-adjusted model: AR = Δyield - mean(Δyield in [-220,-20] window). ",
      "Countries: US, EA, GB, SE. Shaded bands: ±1.96 × propagated SE."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(DIR_FIGURES, "yield_window_paths.pdf"), p_yield_paths, width = 9, height = 6)
ggsave(file.path(DIR_FIGURES, "yield_window_paths.png"), p_yield_paths, width = 9, height = 6, dpi = 150)
log_info("Saved: yield_window_paths.{{pdf,png}}")

# =============================================================================
# Figure 2: CAYC bar chart by event type (easing vs tightening)
# =============================================================================

cayc_plot_df <- results_df |>
  filter(!is.na(cayc_m1_p1)) |>
  mutate(
    direction_label = if_else(direction == 1, "Easing", "Tightening"),
    event_label     = recode(event_type, !!!event_type_labels)
  ) |>
  pivot_longer(
    cols      = c(cayc_m1_p1, cayc_d0_p5),
    names_to  = "window_stem",
    values_to = "cayc"
  ) |>
  mutate(window_label = recode(window_stem, cayc_m1_p1 = "CAYC[-1,+1]", cayc_d0_p5 = "CAYC[0,+5]")) |>
  group_by(event_label, direction_label, window_label) |>
  summarise(
    mean_cayc = mean(cayc, na.rm = TRUE),
    se_cayc   = sd(cayc,   na.rm = TRUE) / sqrt(sum(!is.na(cayc))),
    n         = n(),
    .groups   = "drop"
  )

p_cayc_bar <- cayc_plot_df |>
  ggplot(aes(x = event_label, y = mean_cayc, fill = direction_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = mean_cayc - 1.96 * se_cayc, ymax = mean_cayc + 1.96 * se_cayc),
    position = position_dodge(width = 0.75), width = 0.3
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  facet_wrap(~window_label, nrow = 1) +
  scale_fill_manual(values = c(Easing = "#2166ac", Tightening = "#d6604d")) +
  labs(
    title   = "Average Cumulative Abnormal Yield Change by QE Event Type",
    x       = NULL,
    y       = "Mean CAYC (basis points)",
    fill    = "Direction",
    caption = "Error bars: ±1.96 SE. Negative CAYC = yields fell (easing). Countries: US, EA, GB, SE."
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom")

ggsave(file.path(DIR_FIGURES, "yield_car_by_eventtype.pdf"), p_cayc_bar, width = 10, height = 6)
ggsave(file.path(DIR_FIGURES, "yield_car_by_eventtype.png"), p_cayc_bar, width = 10, height = 6, dpi = 150)
log_info("Saved: yield_car_by_eventtype.{{pdf,png}}")

# =============================================================================
# Figure 3: Joint equity CAR vs bond CAYC scatter (if equity results available)
# =============================================================================

equity_results_path <- file.path(DIR_LOGS, "event_study_results.csv")

if (file.exists(equity_results_path)) {
  equity_df <- read_csv(equity_results_path, show_col_types = FALSE)

  joint_df <- inner_join(
    results_df  |> select(event_id, country, event_type, direction, program_name,
                           surprise, cayc_m1_p1, cayc_d0_p5),
    equity_df   |> select(event_id, car_m1_p1, car_d0_p5),
    by = "event_id"
  ) |>
  mutate(
    car_m1_p1_pct  = car_m1_p1  * 100,
    car_d0_p5_pct  = car_d0_p5  * 100,
    event_label    = recode(event_type, !!!event_type_labels),
    direction_label = if_else(direction == 1, "Easing", "Tightening")
  )

  if (nrow(joint_df) >= 4) {
    p_joint <- joint_df |>
      ggplot(aes(x = cayc_m1_p1, y = car_m1_p1_pct,
                 colour = event_label, shape = direction_label)) +
      geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
      geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
      geom_point(size = 3, alpha = 0.85) +
      geom_text(
        aes(label = program_name),
        size = 2.5, nudge_y = 0.3, check_overlap = TRUE
      ) +
      scale_shape_manual(values = c(Easing = 16, Tightening = 4)) +
      labs(
        title    = "Equity Abnormal Return vs Bond Yield Change Around QE Announcements",
        subtitle = "Each point = one event. Window: [-1, +1] trading days.",
        x        = "CAYC[-1,+1] (basis points; negative = yield fell)",
        y        = "Equity CAR[-1,+1] (%)",
        colour   = "Event type",
        shape    = "Direction",
        caption  = paste0(
          "Bond CAYC from mean-adjusted model (countries: US, EA, GB, SE). ",
          "Equity CAR from market model (S&P 500 benchmark). ",
          "Top-left quadrant: yield fell + equities rose (both channels active)."
        )
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")

    ggsave(file.path(DIR_FIGURES, "equity_vs_yield.pdf"), p_joint, width = 10, height = 7)
    ggsave(file.path(DIR_FIGURES, "equity_vs_yield.png"), p_joint, width = 10, height = 7, dpi = 150)
    log_info("Saved: equity_vs_yield.{{pdf,png}}")
  }
}

# =============================================================================
# Text report
# =============================================================================

report_lines <- c(
  strrep("=", 72),
  "BOND YIELD EVENT STUDY REPORT — QE Announcements",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  strrep("=", 72),
  "",
  "METHODOLOGY",
  "  Mean-adjusted model: AR_i,t = Δy_i,t - mean(Δy_i in estimation window)",
  "  Units: basis points (1 bp = 0.01 percentage points)",
  "  Sign: CAYC < 0 = yields fell (bond prices rose) = expected for easing",
  glue("  Estimation window: [{EST_WIN_START}, {EST_WIN_END}] trading days"),
  glue("  Event window:      [{EVT_WIN_START}, {EVT_WIN_END}] trading days"),
  "",
  "COVERAGE",
  glue("  Countries with daily yield data: {paste(available_countries, collapse=', ')}"),
  glue("  Events covered:  {nrow(results_df)}"),
  glue("  Events excluded: {nrow(events_excluded)} (no yield data for JP, CH)"),
  glue("  Easing events:   {sum(results_df$direction == 1, na.rm=TRUE)}"),
  glue("  Tightening:      {sum(results_df$direction == -1, na.rm=TRUE)}"),
  ""
)

for (etype in sort(unique(results_df$event_type[results_df$direction == 1]))) {
  sub <- results_df |> filter(event_type == etype, direction == 1)
  report_lines <- c(
    report_lines,
    glue("  {etype}  (n = {nrow(sub)})"),
    sprintf(
      "    CAYC[-1,+1] = %+.1fbp  |  CAYC[0,+5] = %+.1fbp  |  CAYC[0,+10] = %+.1fbp",
      mean(sub$cayc_m1_p1,  na.rm = TRUE),
      mean(sub$cayc_d0_p5,  na.rm = TRUE),
      mean(sub$cayc_d0_p10, na.rm = TRUE)
    ),
    ""
  )
}

report_lines <- c(
  report_lines,
  strrep("-", 95),
  sprintf("%-4s  %-10s  %-4s  %-14s  %-14s  %10s  %9s  %10s  %s",
          "ID","Date","Ctry","Program","Type","CAYC[-1+1]","CAYC[0+5]","CAYC[0+10]","t[-1+1]"),
  strrep("-", 95)
)

for (i in seq_len(nrow(results_df))) {
  r   <- results_df[i, ]
  sig <- dplyr::case_when(
    abs(r$t_m1_p1) >= 2.576 ~ "***",
    abs(r$t_m1_p1) >= 1.960 ~ "**",
    abs(r$t_m1_p1) >= 1.645 ~ "*",
    TRUE                     ~ ""
  )
  report_lines <- c(
    report_lines,
    sprintf("%-4s  %-10s  %-4s  %-14s  %-14s  %+9.1fbp  %+8.1fbp  %+9.1fbp  %+6.2f%s",
      r$event_id, format(r$date), r$country, r$program_name, r$event_type,
      coalesce(r$cayc_m1_p1,  NA_real_),
      coalesce(r$cayc_d0_p5,  NA_real_),
      coalesce(r$cayc_d0_p10, NA_real_),
      coalesce(r$t_m1_p1,     NA_real_),
      sig)
  )
}

report_lines <- c(
  report_lines, "",
  "* p<0.10  ** p<0.05  *** p<0.01 (two-tailed)",
  "",
  strrep("-", 72), "LIMITATIONS", strrep("-", 72),
  "1. JP and CH yield data unavailable. These 9 events are equity-only.",
  "2. Mean-adjusted model ignores cross-country yield spillovers. For a richer",
  "   model, substitute a two-factor specification with US 10Y + global PC1.",
  "3. US CAYC values directly reflect the policy shock with no circularity (US",
  "   yield is the *dependent* variable, not the benchmark). The mean-adjusted",
  "   approach is appropriate here; no correction needed.",
  ""
)

report_path <- file.path(DIR_LOGS, "yield_study_report.txt")
writeLines(report_lines, report_path)
log_info("Saved: yield_study_report.txt")

log_info("=== 10_yield_event_study.R complete ===")
log_info("Outputs:")
log_info("  {DIR_FIGURES}/yield_window_paths.{{pdf,png}}")
log_info("  {DIR_FIGURES}/yield_car_by_eventtype.{{pdf,png}}")
log_info("  {DIR_FIGURES}/equity_vs_yield.{{pdf,png}}")
log_info("  {DIR_LOGS}/yield_study_results.csv")
log_info("  {DIR_LOGS}/yield_study_report.txt")
