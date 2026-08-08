# TB TRIAGE+ comparative cost analysis

R code for the prospective two-country comparative cost analysis of two
community-based TB active case-finding screening approaches (CAD4TBv7 alone
versus CAD4TBv7 combined with point-of-care CRP testing) in South Africa and
Lesotho.

Perspective: programme/provider. Currency: 2023 USD. Financial and economic
costs reported separately; capital costs annualised at 3%. Probabilistic
sensitivity analysis (10,000 simulations) with an assumed 20% coefficient of
variation (WHO-CHOICE).

## Requirements

- R (>= 4.1 recommended)
- Packages: tidyverse, data.table, reshape2, gridExtra, ggrepel, treemap,
  plotly, crosstalk, scales, corrplot, patchwork

Install packages once:

```r
install.packages(c("tidyverse", "data.table", "reshape2", "gridExtra",
                   "ggrepel", "treemap", "plotly", "crosstalk", "scales",
                   "corrplot", "patchwork"))
```

## How to run

Open `triage-costing.Rproj` in RStudio first, so the working directory is the
repository root. All paths in the scripts are relative to that root. The
`output/` and `Figures/` directories are created automatically on first run.

Run the whole pipeline:

```r
source("run-all.R")
```

Or run scripts individually, in this order:

| Order | Script | Produces | Depends on |
|-------|--------|----------|------------|
| 1 | `00-base-functions.R` | packages, constants, helper functions | (sourced by every other script) |
| 2 | `01a-costs-southafrica.R` | `output/zaf_costs_*.RDS`, `output/cost_corr_sa_*`, `output/cost_breakdown_sa_*` | — |
| 3 | `01b-costs-lesotho.R` | `output/ls_costs_*.RDS`, `output/cost_corr_ls_*`, `output/cost_breakdown_ls_*` | — |
| 4 | `02a-aggregate-bycountry.R` | country-level tables and figures | 01a, 01b |
| 5 | `02b-aggregate-overall.R` | overall tables, capital/recurrent split, per-person costs | 01a, 01b |
| 6 | `03a-plots-components.R` | cost-component and cost-driver figures; `output/total_costs_*` | 01a, 01b |
| 7 | `03b-plots-sensitivity.R` | combined component figure, sensitivity figure | 03a |

Scripts 01a and 01b each loop over two population sizes (n = 20,023 and
n = 100,000) and two intervention scopes (entire TB+HIV+NCD screening, and
TB-only). Scripts 02 and 03 read a single scenario at a time via the `n` and
`intervention` flags set near the top of each file.

## Note

- **Exchange rate.** ZAR to USD conversion uses a 2023 market average of
  0.05424. LSL is pegged 1:1 to ZAR, so the same rate applies to Lesotho. The
  value is currently hardcoded at each conversion point in 01a/01b; a named
  constant `zar_to_usd` is defined in `00-base-functions.R` for reference.

## Data availability

No individual participant data are read by these scripts; all cost inputs are
entered as constants. Trial data are archived on Zenodo
(DOI: 10.5281/zenodo.19068196).
