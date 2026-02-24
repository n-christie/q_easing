# q_easing

Empirical research on whether central bank balance sheet expansion (QE)
drives equity valuation multiples (P/E, CAPE) across countries.

**Countries:** US, EA (Euro Area), NO, SE, DK, JP, GB, CH
**Study period:** January 2005 – present
**Unit of observation:** Country × Month

See [DATA_REPORT.md](DATA_REPORT.md) for full documentation, methodology, and results.

---

## Prerequisites

### Software
- R 4.3 or later
- RStudio (recommended) — open `q_easing.Rproj`

### FRED API Key
1. Register at https://fred.stlouisfed.org/docs/api/api_key.html (free)
2. Add to `~/.Renviron`:
   ```
   FRED_API_KEY=your_key_here
   ```
3. Restart R (or run `readRenviron("~/.Renviron")`)

### R Packages
Install all dependencies before first run:
```r
install.packages(c(
  "fredr", "ecb", "wbstats", "httr2", "rvest", "readxl", "haven",
  "tidyverse", "zoo", "lubridate", "logger", "fs", "glue", "here",
  "patchwork", "scales", "gt", "modelsummary", "fixest", "lpirfs",
  "tidyquant", "renv"
))
```

---

## Execution Order

### Panel data pipeline
```r
source("R/01_collect_macro.R")    # BIS CBTA, FRED, World Bank
source("R/02_collect_equity.R")   # Shiller CAPE, OECD PBR
source("R/03_collect_controls.R") # ECB, money supply, Chinn-Ito
source("R/04_clean_merge.R")      # Harmonize, construct variables, build panel
source("R/05_descriptives.R")     # Heatmap, time series, summary stats, report
source("R/06_collect_manual.R")   # SSB (NO), Riksbank (SE), DST (DK), Damodaran P/E
```

### Event study
```r
source("R/07_collect_event_data.R")  # Daily equity prices (Stooq)
source("R/08_event_study.R")         # Equity market model event study
source("R/09_collect_yield_data.R")  # Daily 10Y bond yields (UST, ECB, BoE, Riksbank)
source("R/10_yield_event_study.R")   # Bond yield mean-adjusted event study
```

`R/00_config.R` is sourced automatically by each script — do not run it separately.

---

## Project Structure

```
q_easing/
├── R/
│   ├── 00_config.R               # Global config, helpers, logging
│   ├── 01_collect_macro.R        # CB balance sheets, GDP, CPI, yields
│   ├── 02_collect_equity.R       # CAPE and P/E data
│   ├── 03_collect_controls.R     # ECB data, money supply, Chinn-Ito
│   ├── 04_clean_merge.R          # Harmonize, merge, variable construction
│   ├── 05_descriptives.R         # Summary stats, heatmap, availability report
│   ├── 06_collect_manual.R       # Manual data supplement
│   ├── 07_collect_event_data.R   # Daily equity prices for event study
│   ├── 08_event_study.R          # Equity market model event study
│   ├── 09_collect_yield_data.R   # Daily 10Y yield data for yield event study
│   └── 10_yield_event_study.R    # Bond yield mean-adjusted event study
├── data/
│   ├── manual/                   # Hand-curated CSVs and event database (versioned)
│   ├── raw/                      # API downloads — gitignored, regenerable
│   └── processed/                # Derived panel data — gitignored, regenerable
├── output/
│   ├── figures/                  # PNGs versioned; PDFs gitignored
│   └── logs/                     # Result CSVs/TXTs versioned; .log files gitignored
├── DATA_REPORT.md                # Full documentation with embedded figures
└── q_easing.Rproj
```

---

## Key Outputs

| File | Description |
|------|-------------|
| `data/processed/panel_monthly.{rds,csv}` | Final panel: 2,032 rows (8 countries × 254 months) |
| `output/figures/availability_heatmap.png` | Variable coverage by country |
| `output/figures/event_window_paths.png` | Equity CAR paths around QE announcements |
| `output/figures/car_by_eventtype.png` | Average equity CARs by event type |
| `output/figures/spillover_heatmap.png` | Cross-country spillover matrix |
| `output/figures/yield_window_paths.png` | Bond yield CAYC paths around QE announcements |
| `output/figures/yield_car_by_eventtype.png` | Average bond CAYCs by event type |
| `output/figures/equity_vs_yield.png` | Equity CAR vs bond CAYC scatter |
| `output/logs/event_study_results.csv` | Full equity event study results |
| `output/logs/yield_study_results.csv` | Full bond yield event study results |

---

## Data Availability Summary

| Source | Variables | Countries | Status |
|--------|-----------|-----------|--------|
| BIS CBTA | CB total assets | US, EA, JP, GB, CH | Auto |
| FRED | GDP, CPI, yields, VIX, FX, money supply | US/global | Auto (requires API key) |
| World Bank | GDP nominal USD | All | Auto |
| Shiller | US CAPE | US | Auto |
| ECB SDW | EA CPI, yields, M3, assets | EA | Auto |
| Chinn-Ito | Financial openness index | US, NO, SE, DK, JP, GB, CH | Auto |
| Stooq | Daily equity index prices | All 8 | Auto (scripts 07–08) |
| US Treasury | Daily 10Y yield | US | Auto (script 09) |
| BoE IADB | Daily 10Y gilt yield | GB | Auto (script 09) |
| Riksbank API | Daily 10Y yield | SE | Auto (script 09) |
| Manual: SSB | Norges Bank assets | NO | **Manual download required** |
| Manual: Riksbank | Riksbank assets | SE | **Manual download required** |
| Manual: DST | Danmarks Nationalbank assets | DK | **Manual download required** |
| Manual: Star Capital/MSCI | Non-US CAPE | EA, NO, SE, DK, JP, GB, CH | **Outstanding gap** |

---

## Known Gaps

| Issue | Resolution |
|-------|------------|
| Non-US CAPE not freely available | Manual entry from Keimling/Barclays/MSCI (see DATA_REPORT.md §10) |
| JP and CH daily yield data inaccessible | Excluded from yield event study; equity study unaffected |
| BIS CBTA may not cover NO/SE/DK | Manual files in `data/manual/` fill the gap |
| EA GDP from World Bank (`EMU`) may be sparse | Script uses spline interpolation |
