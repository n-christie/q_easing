# =============================================================================
# 05_descriptives.R — Summary Statistics and Availability Report
# Produces: availability_heatmap.{pdf,png}, timeseries_panels.pdf,
#           summary_stats.html, data_availability_report.txt
# =============================================================================

source(here::here("R/00_config.R"))

suppressPackageStartupMessages({
  library(patchwork)
  library(scales)
  library(gt)
  library(modelsummary)
})

# =============================================================================
# Load panel and availability records
# =============================================================================

log_info("=== Loading panel and availability records ===")

load_if_exists <- function(filename_stem) {
  path <- file.path(DIR_PROCESSED, paste0(filename_stem, ".rds"))
  if (file.exists(path)) readRDS(path) else { log_warn("Not found: {path}"); NULL }
}

panel           <- load_if_exists("panel_monthly")
coverage        <- load_if_exists("coverage_summary")
avail_01        <- load_if_exists("availability_01_macro")
avail_02        <- load_if_exists("availability_02_equity")
avail_03        <- load_if_exists("availability_03_controls")

if (is.null(panel)) {
  log_error("Panel not found — run 04_clean_merge.R first")
  stop("panel_monthly.rds not found in data/processed/")
}

log_info("Panel loaded: {nrow(panel)} rows, {ncol(panel)} columns")

# =============================================================================
# 1. Availability Heatmap
# =============================================================================

log_info("=== 1. Availability heatmap ===")

key_vars <- c(
  "cb_gdp_ratio", "delta_cb_gdp",
  "cape", "pe_ratio",
  "cpi_yoy", "yield_10y", "real_rate",
  "vix", "gdp_nominal_usd",
  "kaopen", "excess_money_growth"
)

var_labels <- c(
  cb_gdp_ratio        = "CB/GDP ratio",
  delta_cb_gdp        = "Delta CB/GDP (YoY)",
  cape                = "CAPE",
  pe_ratio            = "P/E ratio",
  cpi_yoy             = "CPI YoY",
  yield_10y           = "10Y yield",
  real_rate           = "Real rate",
  vix                 = "VIX",
  gdp_nominal_usd     = "GDP (nominal USD)",
  kaopen              = "Chinn-Ito (kaopen)",
  excess_money_growth = "Excess money growth"
)

heatmap_data <- panel |>
  group_by(country) |>
  summarise(
    across(
      all_of(intersect(key_vars, names(panel))),
      ~ round(mean(!is.na(.x)) * 100, 1),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(-country, names_to = "variable", values_to = "pct_available") |>
  mutate(
    variable = factor(
      variable,
      levels = key_vars,
      labels = var_labels[key_vars]
    ),
    country  = factor(country, levels = rev(COUNTRIES))
  )

heatmap_plot <- ggplot(heatmap_data, aes(x = variable, y = country, fill = pct_available)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct_available, "%")),
            size = 3, color = ifelse(heatmap_data$pct_available > 50, "white", "black")) +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "#fee08b",
    high     = "#1a9850",
    midpoint = 50,
    limits   = c(0, 100),
    name     = "% Available"
  ) +
  labs(
    title    = "Data Availability Heatmap",
    subtitle = glue("Study period: {STUDY_START} to {STUDY_END} | Countries: {paste(COUNTRIES, collapse=', ')}"),
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    plot.margin      = margin(10, 10, 20, 10)
  )

# Save as PDF and PNG
heatmap_pdf <- file.path(DIR_FIGURES, "availability_heatmap.pdf")
heatmap_png <- file.path(DIR_FIGURES, "availability_heatmap.png")

ggsave(heatmap_pdf, heatmap_plot, width = 12, height = 6)
ggsave(heatmap_png, heatmap_plot, width = 12, height = 6, dpi = 150)
log_info("Heatmap saved: {heatmap_pdf}, {heatmap_png}")

# =============================================================================
# 2. Time Series Plots (patchwork)
# =============================================================================

log_info("=== 2. Time series plots ===")

country_colors <- setNames(
  scales::hue_pal()(length(COUNTRIES)),
  COUNTRIES
)

ts_vars <- list(
  list(var = "cb_gdp_ratio",    label = "CB Assets / GDP (%)",         title = "Central Bank Balance Sheet (% of GDP)"),
  list(var = "cape",            label = "CAPE",                         title = "Cyclically Adjusted P/E Ratio (CAPE)"),
  list(var = "real_rate",       label = "Real Interest Rate (pp)",      title = "Real 10-Year Interest Rate"),
  list(var = "cpi_yoy",         label = "CPI YoY (%)",                  title = "CPI Inflation (Year-on-Year)"),
  list(var = "gdp_yoy",         label = "GDP YoY (%)",                  title = "GDP Growth (Year-on-Year)"),
  list(var = "vix",             label = "VIX",                          title = "VIX Volatility Index (Global)")
)

make_ts_plot <- function(spec) {
  if (!spec$var %in% names(panel)) {
    return(ggplot() + labs(title = glue("{spec$title} — NOT AVAILABLE")) + theme_void())
  }

  plot_data <- panel |>
    filter(!is.na(.data[[spec$var]])) |>
    select(date, country, value = all_of(spec$var))

  if (nrow(plot_data) == 0) {
    return(ggplot() + labs(title = glue("{spec$title} — NO DATA")) + theme_void())
  }

  ggplot(plot_data, aes(x = date, y = value, color = country)) +
    geom_line(linewidth = 0.6, alpha = 0.85) +
    scale_color_manual(values = country_colors) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
      title  = spec$title,
      x      = NULL,
      y      = spec$label,
      color  = "Country"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position  = "right",
      legend.key.size  = unit(0.4, "cm"),
      legend.text      = element_text(size = 7),
      plot.title       = element_text(face = "bold", size = 9),
      axis.text.x      = element_text(angle = 30, hjust = 1)
    )
}

ts_plots <- map(ts_vars, make_ts_plot)
ts_combined <- wrap_plots(ts_plots, ncol = 2) +
  plot_annotation(
    title    = "QE and Equity Valuation - Key Time Series",
    subtitle = glue("Study period: {STUDY_START} to {STUDY_END}"),
    theme    = theme(plot.title = element_text(face = "bold", size = 13))
  )

ts_pdf <- file.path(DIR_FIGURES, "timeseries_panels.pdf")
ggsave(ts_pdf, ts_combined, width = 14, height = 10)
log_info("Time series plots saved: {ts_pdf}")

# =============================================================================
# 3. Summary Statistics Table
# =============================================================================

log_info("=== 3. Summary statistics table ===")

stats_vars <- intersect(
  c("cb_gdp_ratio", "delta_cb_gdp", "cape", "pe_ratio",
    "cpi_yoy", "yield_10y", "real_rate", "vix",
    "gdp_nominal_usd", "kaopen"),
  names(panel)
)

summary_tbl <- panel |>
  select(country, all_of(stats_vars)) |>
  group_by(country) |>
  summarise(
    across(
      all_of(stats_vars),
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 2),
        sd   = ~ round(sd(.x,   na.rm = TRUE), 2),
        min  = ~ round(min(.x,  na.rm = TRUE), 2),
        max  = ~ round(max(.x,  na.rm = TRUE), 2),
        n    = ~ sum(!is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

# Reshape to long for better gt presentation
summary_long <- summary_tbl |>
  pivot_longer(-country, names_to = "col", values_to = "val") |>
  separate(col, into = c("variable", "stat"), sep = "__") |>
  pivot_wider(names_from = stat, values_from = val)

gt_tbl <- summary_long |>
  arrange(variable, country) |>
  gt(groupname_col = "variable") |>
  tab_header(
    title    = "Summary Statistics by Country",
    subtitle = glue("Study period: {STUDY_START} to {STUDY_END}")
  ) |>
  cols_label(
    country  = "Country",
    mean     = "Mean",
    sd       = "SD",
    min      = "Min",
    max      = "Max",
    n        = "N (non-NA)"
  ) |>
  fmt_number(columns = c(mean, sd, min, max), decimals = 2) |>
  fmt_integer(columns = n) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  opt_table_font(font = "Courier New") |>
  tab_options(table.font.size = px(11))

html_path <- file.path(DIR_FIGURES, "summary_stats.html")
gtsave(gt_tbl, html_path)
log_info("Summary stats table saved: {html_path}")

# =============================================================================
# 4. Text Availability Report
# =============================================================================

log_info("=== 4. Availability report ===")

report_path <- file.path(DIR_LOGS, "data_availability_report.txt")

format_avail <- function(avail_list, section_name) {
  sep <- strrep("=", 60)
  lines <- c(sep, section_name, sep)
  if (is.null(avail_list)) {
    lines <- c(lines, "  [No availability data — script may not have been run]")
    return(lines)
  }
  for (nm in names(avail_list)) {
    item <- avail_list[[nm]]
    lines <- c(lines, glue("  [{nm}]"))
    if (is.list(item)) {
      for (field in names(item)) {
        val <- item[[field]]
        if (is.character(val) && length(val) > 1) val <- paste(val, collapse = ", ")
        if (is.numeric(val)) val <- round(val, 3)
        lines <- c(lines, glue("    {field}: {paste(val, collapse=' ')}"))
      }
    } else {
      lines <- c(lines, glue("    {item}"))
    }
  }
  lines
}

# Per-variable coverage from panel
var_coverage <- panel |>
  summarise(across(
    where(is.numeric),
    ~ paste0(round(mean(!is.na(.x)) * 100, 1), "% available (",
             sum(!is.na(.x)), "/", n(), " obs)")
  )) |>
  pivot_longer(everything(), names_to = "variable", values_to = "coverage")

# Manual file presence check
manual_files <- c(
  "cb_assets_NO.csv"       = file.path(DIR_MANUAL, "cb_assets_NO.csv"),
  "cb_assets_SE.csv"       = file.path(DIR_MANUAL, "cb_assets_SE.csv"),
  "cb_assets_DK.csv"       = file.path(DIR_MANUAL, "cb_assets_DK.csv"),
  "equity_valuation.csv"   = file.path(DIR_MANUAL, "equity_valuation.csv")
)
manual_status <- map_chr(manual_files, ~ if (file.exists(.x)) "PRESENT" else "MISSING")

h1 <- strrep("#", 60)
h2 <- strrep("=", 60)

report_lines <- c(
  h1,
  "QE EASING RESEARCH PROJECT -- DATA AVAILABILITY REPORT",
  glue("Generated: {Sys.time()}"),
  glue("Study period: {STUDY_START} to {STUDY_END}"),
  glue("Countries: {paste(COUNTRIES, collapse=', ')}"),
  h1,
  "",
  format_avail(avail_01, "SCRIPT 01: Macro Data (BIS CBTA, FRED, World Bank)"),
  "",
  format_avail(avail_02, "SCRIPT 02: Equity Valuation (Shiller, OECD PBR)"),
  "",
  format_avail(avail_03, "SCRIPT 03: Controls (ECB, Money Supply, Chinn-Ito)"),
  "",
  h2,
  "PANEL VARIABLE COVERAGE (% non-NA obs)",
  h2,
  paste0("  ", var_coverage$variable, ": ", var_coverage$coverage),
  "",
  h2,
  "MANUAL DATA FILES",
  h2,
  paste0("  ", names(manual_status), ": ", manual_status),
  "",
  h2,
  "ACTION ITEMS",
  h2,
  if (manual_status["cb_assets_NO.csv"] == "MISSING")
    "  [MISSING] cb_assets_NO.csv -- download from SSB Statbank table 08428",
  if (manual_status["cb_assets_SE.csv"] == "MISSING")
    "  [MISSING] cb_assets_SE.csv -- download from Riksbank balance sheet page",
  if (manual_status["cb_assets_DK.csv"] == "MISSING")
    "  [MISSING] cb_assets_DK.csv -- download from Danmarks Nationalbank statistikbank",
  if (manual_status["equity_valuation.csv"] == "MISSING")
    "  [MISSING] equity_valuation.csv -- enter non-US CAPE from Star Capital or MSCI",
  "  See data/manual/README.md for column specifications and download links.",
  "",
  h1,
  "END OF REPORT",
  h1
)

writeLines(report_lines, report_path)
log_info("Availability report saved: {report_path}")

# Print to console as well
cat(paste(report_lines, collapse = "\n"))

log_info("Script 05 complete.")
