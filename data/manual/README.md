# Manual Data Download Instructions

These files must be downloaded manually and placed in this directory (`data/manual/`).
After adding them, rerun `R/04_clean_merge.R` and `R/05_descriptives.R`.

---

## 1. Norges Bank Balance Sheet → `cb_assets_NO.csv`

**Source:** Statistics Norway (SSB) Statbank
**URL:** https://www.ssb.no/en/statbank/table/08428
- Select: Norges Bank, total assets, monthly, from January 2005
- Export as CSV

**Required column format:**

```
date,cb_assets
2005-01-01,1234567
2005-02-01,1345678
...
```

| Column | Type | Description |
|--------|------|-------------|
| `date` | YYYY-MM-DD | First day of month |
| `cb_assets` | numeric | Total assets in **millions NOK** |

---

## 2. Riksbank Balance Sheet → `cb_assets_SE.csv`

**Source:** Riksbank
**URL:** https://www.riksbank.se/en-gb/markets/riksbanks-balance-sheet/

- Navigate to "Balance sheet" → download historical monthly data
- Or use the Riksbank's statistical database: https://www.riksbank.se/en-gb/statistics/

**Required column format:**

```
date,cb_assets
2005-01-01,1234567
2005-02-01,1345678
...
```

| Column | Type | Description |
|--------|------|-------------|
| `date` | YYYY-MM-DD | First day of month |
| `cb_assets` | numeric | Total assets in **millions SEK** |

---

## 3. Danmarks Nationalbank Balance Sheet → `cb_assets_DK.csv`

**Source:** Danmarks Nationalbank Statistical Database
**URL:** https://nationalbanken.statistikbank.dk

- Navigate to "Balance sheet of Danmarks Nationalbank" (Danish: Nationalbankens balance)
- Select monthly frequency, total assets, from January 2005

**Required column format:**

```
date,cb_assets
2005-01-01,1234567
2005-02-01,1345678
...
```

| Column | Type | Description |
|--------|------|-------------|
| `date` | YYYY-MM-DD | First day of month |
| `cb_assets` | numeric | Total assets in **millions DKK** |

---

## 4. Non-US Equity Valuation → `equity_valuation.csv`

**Source options:**
- **Star Capital:** https://www.starcapital.de (CAPE by country, monthly)
- **MSCI Monthly Factsheets:** https://www.msci.com/real-time-index-data-search (P/E forward and trailing)
- **Barclays/Research Affiliates** published CAPE estimates

**Countries needed:** EA, NO, SE, DK, JP, GB, CH

**Note on EA CAPE:** The Euro Area does not have a single widely-published CAPE.
Options in decreasing order of quality:
1. GDP-weighted average of Germany (DE) + France (FR) + Italy (IT) + Spain (ES)
2. Germany (DE) as proxy — largest EA economy
3. MSCI EMU index P/E from MSCI factsheets

**Required column format:**

```
date,country,cape,pe_trailing,pe_forward
2005-01-01,EA,16.2,,
2005-01-01,JP,22.1,24.3,18.5
2005-01-01,GB,17.8,19.2,15.1
...
```

| Column | Type | Description |
|--------|------|-------------|
| `date` | YYYY-MM-DD | First day of month |
| `country` | character | 2-letter code: EA, NO, SE, DK, JP, GB, CH |
| `cape` | numeric | Cyclically adjusted P/E (Shiller CAPE, 10-year real earnings) |
| `pe_trailing` | numeric | Trailing 12-month P/E (optional) |
| `pe_forward` | numeric | Forward 12-month P/E (optional) |

**IMPORTANT:** Leave columns blank (not 0) if data is unavailable for a given country/date.
The pipeline will treat blanks as `NA`.

---

## CSV Template Files

### `cb_assets_XX.csv` template:
```
date,cb_assets
2005-01-01,
2005-02-01,
```

### `equity_valuation.csv` template:
```
date,country,cape,pe_trailing,pe_forward
2005-01-01,EA,,,
2005-01-01,NO,,,
2005-01-01,SE,,,
2005-01-01,DK,,,
2005-01-01,JP,,,
2005-01-01,GB,,,
2005-01-01,CH,,,
```

---

## Verification

After adding files, run:
```r
source("R/04_clean_merge.R")
source("R/05_descriptives.R")
```

Then open `output/logs/data_availability_report.txt` to confirm the files are picked up.
The heatmap in `output/figures/availability_heatmap.png` should show improved coverage
for the relevant countries.
