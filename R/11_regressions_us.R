# =============================================================================
# 11_regressions_us.R — US-only time series regressions
#
# Runs four OLS specifications and a Jordà (2005) local projection impulse
# response for the US, using Newey-West HAC standard errors throughout.
#
# Outputs:
#   output/logs/us_ols_results.csv        — OLS table
#   output/logs/us_lp_results.csv         — LP coefficients by horizon
#   output/logs/us_regression_report.txt  — narrative summary
#   output/figures/us_ols_coefplot.{pdf,png}
#   output/figures/us_lp_irf.{pdf,png}
# =============================================================================

source("R/00_config.R")

suppressPackageStartupMessages({
  library(sandwich)    # NeweyWest()
  library(lmtest)      # coeftest()
  library(stargazer)   # regression tables
  library(broom)       # tidy()
  library(patchwork)   # combine figures
})

log_info("=== 11_regressions_us.R: US-only regressions ===")

# -----------------------------------------------------------------------------
# 1. Load and prepare data
# -----------------------------------------------------------------------------

panel <- readRDS(file.path(DIR_PROCESSED, "panel_monthly.rds"))

us <- panel |>
  filter(country == "US") |>
  arrange(date) |>
  mutate(
    trend    = as.numeric(date - min(date)) / 365.25,  # years since 2005-01
    d_cape   = cape - lag(cape),                        # monthly ΔCAPE
    cape_lag1 = lag(cape, 1),
    dcb_lag1  = lag(delta_cb_gdp, 1),
    dcb_lag2  = lag(delta_cb_gdp, 2)
  ) |>
  filter(!is.na(cape), !is.na(cb_gdp_ratio), !is.na(delta_cb_gdp),
         !is.na(real_rate), !is.na(cpi_yoy), !is.na(gdp_yoy), !is.na(vix))

log_info("US complete cases: {nrow(us)} months ({format(min(us$date), '%Y-%m')} to {format(max(us$date), '%Y-%m')})")

# -----------------------------------------------------------------------------
# 2. OLS regressions (four specifications)
# -----------------------------------------------------------------------------
# All use Newey-West HAC SEs with 12-month lag bandwidth.
#
# Spec 1: CB/GDP level — raw correlation (likely upward-biased by common trend)
# Spec 2: ΔCB/GDP (YoY change) — less trended, closer to a flow measure
# Spec 3: CB/GDP level + linear time trend — partial detrending
# Spec 4: ΔCAPE as dependent variable — most stationary, short-run dynamics

m1 <- lm(cape   ~ cb_gdp_ratio + real_rate + cpi_yoy + gdp_yoy + vix,         data = us)
m2 <- lm(cape   ~ delta_cb_gdp + real_rate + cpi_yoy + gdp_yoy + vix,         data = us)
m3 <- lm(cape   ~ cb_gdp_ratio + real_rate + cpi_yoy + gdp_yoy + vix + trend, data = us)
m4 <- lm(d_cape ~ delta_cb_gdp + real_rate + cpi_yoy + gdp_yoy + vix,         data = us)

nw12 <- function(m) NeweyWest(m, lag = 12, prewhite = FALSE)

models <- list(
  "(1) Level CB/GDP"  = m1,
  "(2) Δ CB/GDP"      = m2,
  "(3) Level + trend" = m3,
  "(4) Δ CAPE"        = m4
)

# Newey-West SEs and p-values for each model (passed directly to stargazer)
se_nw <- lapply(list(m1, m2, m3, m4), function(m)
  sqrt(diag(NeweyWest(m, lag = 12, prewhite = FALSE))))

p_nw <- lapply(seq_along(list(m1, m2, m3, m4)), function(i) {
  m  <- list(m1, m2, m3, m4)[[i]]
  ct <- coeftest(m, vcov = NeweyWest(m, lag = 12, prewhite = FALSE))
  ct[, "Pr(>|t|)"]
})

coef_labels <- c(
  "CB/GDP ratio (%)",
  "\\$\\Delta\\$ CB/GDP (YoY pp)",
  "Real 10Y rate (%)",
  "CPI inflation YoY (%)",
  "Real GDP growth YoY (%)",
  "VIX",
  "Time trend (years)"
)

# HTML table — for standalone viewing
stargazer(
  m1, m2, m3, m4,
  se             = se_nw,
  p              = p_nw,
  type           = "html",
  out            = file.path(DIR_LOGS, "us_ols_table.html"),
  title          = "US CAPE and Fed Balance Sheet Expansion — OLS Estimates",
  dep.var.labels = c("CAPE", "CAPE", "CAPE", "&Delta; CAPE"),
  column.labels  = c("(1) Level", "(2) &Delta; CB/GDP", "(3) + Trend", "(4) &Delta; CAPE"),
  covariate.labels = coef_labels,
  omit.stat      = c("f", "ser"),
  notes          = "Newey-West HAC standard errors (12-month bandwidth) in parentheses.",
  notes.append   = FALSE,
  star.cutoffs   = c(0.10, 0.05, 0.01)
)
log_info("Saved: us_ols_table.html")

# Plain-text table — for embedding in the report
txt_table <- capture.output(
  stargazer(
    m1, m2, m3, m4,
    se             = se_nw,
    p              = p_nw,
    type           = "text",
    title          = "US CAPE and Fed Balance Sheet Expansion",
    dep.var.labels = c("CAPE", "CAPE", "CAPE", "D.CAPE"),
    column.labels  = c("(1) Level", "(2) D.CB/GDP", "(3) +Trend", "(4) D.CAPE"),
    covariate.labels = c(
      "CB/GDP ratio (%)", "D.CB/GDP (YoY pp)", "Real 10Y rate (%)",
      "CPI infl. YoY (%)", "GDP growth YoY (%)", "VIX", "Trend (yrs)"
    ),
    omit.stat      = c("f", "ser"),
    notes          = "NW-HAC SEs (lag=12) in parentheses. * p<0.1 ** p<0.05 *** p<0.01",
    notes.append   = FALSE,
    star.cutoffs   = c(0.10, 0.05, 0.01)
  )
)
writeLines(txt_table, file.path(DIR_LOGS, "us_ols_table.txt"))
log_info("Saved: us_ols_table.txt")

# Extract key coefficients for the report
extract_coef <- function(model, coef_name, lag = 12) {
  ct <- coeftest(model, vcov = NeweyWest(model, lag = lag, prewhite = FALSE))
  row <- ct[coef_name, ]
  list(beta = row["Estimate"], se = row["Std. Error"],
       t = row["t value"], p = row["Pr(>|t|)"])
}

c1 <- extract_coef(m1, "cb_gdp_ratio")
c2 <- extract_coef(m2, "delta_cb_gdp")
c3 <- extract_coef(m3, "cb_gdp_ratio")
c4 <- extract_coef(m4, "delta_cb_gdp")

# -----------------------------------------------------------------------------
# 3. OLS coefficient plot
# -----------------------------------------------------------------------------

coef_plot_df <- tibble(
  spec  = c("(1) Level CB/GDP\n(dep: CAPE)",
            "(2) Δ CB/GDP\n(dep: CAPE)",
            "(3) Level + trend\n(dep: CAPE)",
            "(4) Δ CB/GDP\n(dep: ΔCAPE)"),
  beta  = c(c1$beta, c2$beta, c3$beta, c4$beta),
  se    = c(c1$se,   c2$se,   c3$se,   c4$se),
  xlab  = c("CB/GDP ratio (pp)", "ΔCB/GDP YoY (pp)",
             "CB/GDP ratio (pp)", "ΔCB/GDP YoY (pp)")
) |>
  mutate(
    ci90_lo = beta - qnorm(0.95) * se,
    ci90_hi = beta + qnorm(0.95) * se,
    ci68_lo = beta - qnorm(0.84) * se,
    ci68_hi = beta + qnorm(0.84) * se,
    spec    = factor(spec, levels = rev(spec))
  )

fig_ols <- ggplot(coef_plot_df, aes(y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_linerange(aes(xmin = ci90_lo, xmax = ci90_hi), linewidth = 0.8,
                 colour = "#2166ac") +
  geom_linerange(aes(xmin = ci68_lo, xmax = ci68_hi), linewidth = 2,
                 colour = "#2166ac") +
  geom_point(aes(x = beta), size = 3, colour = "#2166ac") +
  labs(
    title    = "OLS estimates: effect of CB/GDP on US CAPE",
    subtitle = "Point estimate with 68% and 90% Newey-West HAC confidence intervals",
    x        = "Coefficient (CAPE points per 1pp change in CB/GDP measure)",
    y        = NULL,
    caption  = "Controls: real 10Y rate, CPI YoY, GDP YoY, VIX. Spec (3) adds linear time trend."
  ) +
  theme_bw(base_size = 11) +
  theme(plot.caption = element_text(size = 8, colour = "grey40"))

ggsave(file.path(DIR_FIGURES, "us_ols_coefplot.pdf"), fig_ols, width = 7, height = 4)
ggsave(file.path(DIR_FIGURES, "us_ols_coefplot.png"), fig_ols, width = 7, height = 4, dpi = 150)
log_info("Saved: us_ols_coefplot.{{pdf,png}}")

# -----------------------------------------------------------------------------
# 4. Local projection — Jordà (2005)
# -----------------------------------------------------------------------------
# For each horizon h = 0, ..., 24 months, estimate:
#   CAPE_{t+h} - CAPE_{t-1} = alpha + beta_h * ΔCB/GDP_t
#                             + [ΔCB/GDP_{t-1}, ΔCB/GDP_{t-2}]   (lags of treatment)
#                             + [real_rate, cpi_yoy, gdp_yoy, vix]_t  (contemporaneous controls)
#                             + CAPE_{t-1}                         (initial condition)
#                             + epsilon_{t,h}
#
# HAC lag = h+1 (standard choice for LP to account for MA(h) structure of errors)

MAX_H <- 24

lp_results <- map_dfr(0:MAX_H, function(h) {
  us_h <- us |>
    mutate(outcome = lead(cape, h) - cape_lag1) |>
    filter(!is.na(outcome), !is.na(delta_cb_gdp), !is.na(cape_lag1),
           !is.na(real_rate), !is.na(cpi_yoy), !is.na(gdp_yoy), !is.na(vix),
           !is.na(dcb_lag1), !is.na(dcb_lag2))

  if (nrow(us_h) < 30) return(NULL)

  fit <- lm(outcome ~ delta_cb_gdp + dcb_lag1 + dcb_lag2 +
              real_rate + cpi_yoy + gdp_yoy + vix + cape_lag1,
            data = us_h)

  nw_lag <- max(h + 1, 1)
  se_hac <- sqrt(diag(NeweyWest(fit, lag = nw_lag, prewhite = FALSE)))

  beta <- coef(fit)["delta_cb_gdp"]
  se   <- se_hac["delta_cb_gdp"]

  tibble(
    h       = h,
    beta    = beta,
    se      = se,
    ci90_lo = beta - qnorm(0.95) * se,
    ci90_hi = beta + qnorm(0.95) * se,
    ci68_lo = beta - qnorm(0.84) * se,
    ci68_hi = beta + qnorm(0.84) * se,
    n       = nrow(us_h)
  )
})

write.csv(lp_results, file.path(DIR_LOGS, "us_lp_results.csv"), row.names = FALSE)
log_info("Saved: us_lp_results.csv ({nrow(lp_results)} horizons)")

# Peak effect and when it occurs
peak_row  <- lp_results[which.max(lp_results$beta), ]
sig_rows  <- lp_results |> filter(ci90_lo > 0)

# -----------------------------------------------------------------------------
# 5. IRF figure
# -----------------------------------------------------------------------------

fig_lp <- ggplot(lp_results, aes(x = h)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "#2166ac", alpha = 0.15) +
  geom_ribbon(aes(ymin = ci68_lo, ymax = ci68_hi), fill = "#2166ac", alpha = 0.28) +
  geom_line(aes(y = beta), colour = "#2166ac", linewidth = 1) +
  geom_point(aes(y = beta), colour = "#2166ac", size = 2) +
  scale_x_continuous(breaks = seq(0, MAX_H, 6),
                     labels = function(x) paste0("h=", x)) +
  labs(
    title    = "Impulse Response: US CAPE after a 1pp Rise in ΔCB/GDP",
    subtitle = "Local projection (Jordà 2005) | Newey-West HAC SEs | 68% and 90% CIs",
    x        = "Months after shock",
    y        = "Cumulative change in CAPE (points vs. t−1)",
    caption  = paste0(
      "Treatment: +1pp year-on-year change in Fed assets / nominal GDP. ",
      "Outcome: CAPE_{t+h} − CAPE_{t−1}.\n",
      "Controls: real rate, CPI YoY, GDP YoY, VIX, two lags of treatment, lagged CAPE level."
    )
  ) +
  theme_bw(base_size = 11) +
  theme(plot.caption = element_text(size = 8, colour = "grey40"))

ggsave(file.path(DIR_FIGURES, "us_lp_irf.pdf"), fig_lp, width = 8, height = 5)
ggsave(file.path(DIR_FIGURES, "us_lp_irf.png"), fig_lp, width = 8, height = 5, dpi = 150)
log_info("Saved: us_lp_irf.{{pdf,png}}")

# -----------------------------------------------------------------------------
# 6. Narrative report
# -----------------------------------------------------------------------------

fmt <- function(x, d = 2) formatC(round(x, d), format = "f", digits = d)
sig_label <- function(p) {
  if (p < 0.01) "p<0.01 ***" else if (p < 0.05) "p<0.05 **" else
    if (p < 0.10) "p<0.10 *" else "p≥0.10 (n.s.)"
}

report_lines <- c(
  "========================================================================",
  "US TIME SERIES REGRESSIONS — QE and Equity Valuations",
  glue("Generated: {Sys.time()}"),
  "========================================================================",
  "",
  "DATA",
  glue("  Sample: {format(min(us$date), '%Y-%m')} to {format(max(us$date), '%Y-%m')} ({nrow(us)} months)"),
  "  Dependent variable: US CAPE (Shiller cyclically adjusted P/E)",
  "  Key treatment: ΔCB/GDP = year-on-year change in Fed assets as % of GDP (pp)",
  "",
  "OLS RESULTS (Newey-West HAC, 12-month lag bandwidth)",
  "",
  glue("  Spec 1 — Level CB/GDP (dep: CAPE):"),
  glue("    beta = {fmt(c1$beta)} CAPE pts per 1pp CB/GDP | SE={fmt(c1$se)} | {sig_label(c1$p)}"),
  "",
  glue("  Spec 2 — Δ CB/GDP (dep: CAPE):"),
  glue("    beta = {fmt(c2$beta)} CAPE pts per 1pp ΔCB/GDP | SE={fmt(c2$se)} | {sig_label(c2$p)}"),
  "",
  glue("  Spec 3 — Level CB/GDP + time trend (dep: CAPE):"),
  glue("    beta = {fmt(c3$beta)} CAPE pts per 1pp CB/GDP | SE={fmt(c3$se)} | {sig_label(c3$p)}"),
  "",
  glue("  Spec 4 — Δ CB/GDP (dep: ΔCAPE monthly):"),
  glue("    beta = {fmt(c4$beta)} CAPE pts per 1pp ΔCB/GDP | SE={fmt(c4$se)} | {sig_label(c4$p)}"),
  "",
  "LOCAL PROJECTION (Jordà 2005)",
  "",
  glue("  Peak beta_h = {fmt(peak_row$beta)} CAPE pts at h={peak_row$h} months"),
  glue("  Horizons with 90%-CI excluding zero: {if(nrow(sig_rows)>0) paste(sig_rows$h, collapse=', ') else 'none'}"),
  "",
  "INTERPRETATION",
  "",
  "  Spec 1 shows a large positive association between the Fed balance sheet",
  "  and US CAPE — but this is almost certainly driven by a common upward trend",
  "  in both series (balance sheet expansion and rising valuations post-GFC).",
  "",
  "  Spec 3 (level + trend) substantially reduces the coefficient by absorbing",
  "  the secular trend, isolating within-period variation.",
  "",
  "  Spec 2 (ΔCB/GDP) is less affected by trends and represents the short-run",
  "  association between the pace of balance sheet expansion and CAPE.",
  "",
  "  Spec 4 (ΔCAPE) is the most conservative: it asks whether CAPE rises in",
  "  the same month the balance sheet expands faster-than-usual.",
  "",
  "  The local projection traces the dynamic path: a 1pp rise in ΔCB/GDP is",
  glue("  associated with a peak CAPE response of {fmt(peak_row$beta)} points at h={peak_row$h} months,"),
  "  after which the effect fades. Confidence intervals are wide, reflecting",
  "  the limited number of distinct QE episodes in the 2006-2023 sample.",
  "",
  "CAUTION",
  "  These are time series associations for a single country. They cannot",
  "  distinguish CB balance sheet effects from confounders (low rates, strong",
  "  growth) that drove both QE and rising valuations. Causal identification",
  "  requires the full cross-country panel or an IV strategy.",
  "========================================================================"
)

writeLines(report_lines, file.path(DIR_LOGS, "us_regression_report.txt"))
log_info("Saved: us_regression_report.txt")
log_info("=== 11_regressions_us.R complete ===")
