# CLAUDE.md — q_easing Project Briefing

This file is read automatically by Claude Code at session start. It provides
continuity for a multi-session research project on QE and equity valuations.

---

## Project Purpose

Empirical research on whether central bank balance sheet expansion (QE) drives
equity valuation multiples (P/E, CAPE) across 8 countries: US, EA (Euro Area),
NO, SE, DK, JP, GB, CH. Study period: January 2005 – present.

Two main analyses:
1. **Panel event study** — equity CARs and bond yield CAYCs around QE announcements
2. **US regressions** — OLS (4 specs) and Jordà local projections for CAPE on CB/GDP ratio

The primary deliverable is **DATA_REPORT.md** — a full markdown document with
embedded figures, equations (LaTeX math syntax), and results tables. Hosted on
GitHub: https://github.com/n-christie/q_easing

---

## Script Inventory and Status

All scripts source `R/00_config.R` automatically. Run in order.

| Script | Purpose | Status |
|--------|---------|--------|
| `00_config.R` | Global config, helpers, logging | Complete |
| `01_collect_macro.R` | BIS CBTA, FRED, World Bank GDP | Complete |
| `02_collect_equity.R` | Shiller CAPE, equity P/E | Complete |
| `03_collect_controls.R` | ECB SDW, money supply, Chinn-Ito | Complete |
| `04_clean_merge.R` | Harmonize, merge, variable construction | Complete |
| `05_descriptives.R` | Summary stats, heatmap, availability report | Complete |
| `06_collect_manual.R` | SSB (NO), Riksbank (SE), DST (DK), Damodaran | Complete |
| `07_collect_event_data.R` | Daily equity index prices (Stooq) | Complete |
| `08_event_study.R` | Market model equity CAR event study | Complete |
| `09_collect_yield_data.R` | Daily 10Y bond yields (US, EA, GB, SE) | Complete |
| `10_yield_event_study.R` | Mean-adjusted bond yield CAYC event study | Complete |
| `11_regressions_us.R` | US OLS + Jordà LP regressions (stargazer output) | Complete |

**Upcoming work:** Capital IQ data integration (non-US panel regressions, possibly
sector-level analysis). Non-US CAPE data is currently an outstanding gap.

---

## Key Technical Decisions

### Data Sources and Proxies
- **CB balance sheets:** BIS CBTA for US/EA/JP/GB/CH; manual CSVs for NO/SE/DK
  (SSB, Riksbank, Danmarks Nationalbank)
- **EA equity index:** EuroStoxx 50 (^STOXX50E on Stooq) as proxy
- **CAPE:** Shiller for US (auto); non-US CAPE from `data/manual/equity_valuation.csv`
  (partially populated — outstanding gap)
- **Bond yields:** US Treasury monthly CSVs, ECB SDW, BoE IADB, Riksbank REST API.
  JP and CH yield data are unavailable — excluded from yield event study.
- **Financial openness:** Chinn-Ito index (latest vintage from their website)

### Event Study Design
- **Equity:** Market model (OLS), estimation window [-120, -11], event window [-1, +5];
  normal returns from MSCI World index. CAR = cumulative abnormal return.
- **Bond yields:** Mean-adjusted model (Gagnon et al. 2011), estimation window
  [-120, -11], event window [-1, +5]. CAYC = cumulative abnormal yield change
  in basis points. CAYC < 0 means yields fell (expected QE effect).
- **QE events database:** `data/manual/qe_events.csv` — hand-curated, versioned.

### Regression Design (US, script 11)
- **OLS specs:** (1) CAPE ~ CB/GDP ratio + controls; (2) ΔCAPE ~ ΔCB/GDP + controls;
  (3) CAPE ~ CB/GDP + trend + controls; (4) ΔCAPE ~ ΔCB/GDP + controls
- **SEs:** Newey-West HAC (lag=12) via `sandwich::NeweyWest` + `lmtest::coeftest`
- **Tables:** `stargazer` format with custom `se=` and `p=` lists passed for NW SEs
- **Local projection:** Jordà (2005), horizons 0–24 months, HAC lag = h+1

### Coding Conventions
- Always `source("R/00_config.R")` at the top of every script
- Use `DIR_FIGURES`, `DIR_LOGS`, `DIR_PROCESSED` path constants from config
- Use `save_processed()` / `save_raw()` / `safe_fetch()` helpers from config
- Logging via `logger` package — `log_info()`, `log_warn()`, `log_error()`
- No interactive plots — all figures saved as PNG via `ggsave()` to `DIR_FIGURES`

---

## Data Layout

```
data/
  manual/          # Versioned in git — hand-curated source files
    qe_events.csv
    cb_assets_NO/SE/DK.csv
    equity_valuation.csv
  raw/             # Gitignored — regenerable API downloads
  processed/       # Gitignored — derived panel data (panel_monthly, equity_daily, etc.)
```

Large processed files (`panel_monthly.csv`, `equity_daily.csv`, `yield_daily.csv`)
are **not on GitHub** — regenerate by running scripts 01–10 in order.

---

## Git / GitHub

- Repo: https://github.com/n-christie/q_easing (public)
- Branch: `main`
- GitHub user: `n-christie` (not `nchristie`)
- What is committed: R scripts, DATA_REPORT.md, README.md, CLAUDE.md,
  `data/manual/*.csv`, `output/logs/*.csv` (results), `output/figures/*.png`
- What is NOT committed: `data/raw/`, `data/processed/`, `*.rds`, `.log` files,
  `*.pdf`, `.env`, `.claude/`
- Push requires PAT embedded in remote URL (non-interactive terminal). After push,
  reset remote URL to clean form to avoid committing the token.

---

## Known Gaps / Outstanding Work

| Issue | Notes |
|-------|-------|
| Non-US CAPE | `data/manual/equity_valuation.csv` partially filled; Keimling/MSCI/Barclays source |
| JP, CH yield data | Unavailable; excluded from yield event study |
| Non-US regressions | Awaiting Capital IQ data |
| Panel regressions | Next major phase — will need FE/RE models, likely `fixest` |

---

## DATA_REPORT.md Structure (current)

Sections 1–14:
1. Introduction
2. Key Terms and Concepts (glossary)
3. Data Sources
4. Variable Construction
5. QE Events Database
6. Data Availability
7. Equity Event Study Methodology
8. Equity Event Study Results
9. Bond Yield Event Study
10. US Regression Analysis
11. Local Projection (Jordà IRF)
12. Limitations
13. Next Steps (13.1 Panel regressions, 13.2 Capital IQ)
14. Appendix

Equations use `$...$` LaTeX math syntax (renders on GitHub and in VSCode preview).
Figures embedded inline as `![caption](path)` (relative paths from project root).
