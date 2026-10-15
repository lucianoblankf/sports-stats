# Sports stats: goals vs xG of forwards

Analysis of the goals/xG ratio of forwards (Understat, 2014/15 to 2024/25), originally built for a Statistical Methods course in a master's program.

## How to run

Open `Metodos_Estatisticos.Rproj` in RStudio and run `00_run_all.R` (or each script in order).

| Script | What it does |
|---|---|
| `01_download_data.R` | downloads the raw data to `data/raw/` |
| `02_prepare_data.R` | cleans the data (`janitor`) and builds the forward datasets (>= 10 and >= 20 goals) |
| `03_descriptive.R` | descriptive statistics and figures |
| `04_tests_distribution.R` | normality, distribution fitting, hypothesis tests, bootstrap |
| `05_selection_bias.R` | minimum-goals vs minimum-xG cutoffs, plus a bias simulation |
| `06_regression.R` | fixed-effects regressions (`fixest`, `texreg` tables) |
| `07_clusters.R` | finishing profiles via k-means |

Reusable functions live in `R/`. Outputs go to `tabs/` and `figs/`. Old code is kept in `legacy/` (original Portuguese version), and the original course materials are in `docs/`.

## Main results

- g10 dataset (n = 1139): mean goals/xG = 1.156, median 1.122, skewness 1.17.
- With a **minimum-xG** cutoff (which does not select on the outcome) the mean drops to about 0.99 and is no longer different from 1 (xG >= 5: p = 0.16). The "1.16" comes from selection bias.
- Simulation with goals ~ Poisson(xG), i.e. no skill at all: the 10-goal cutoff alone already produces a mean ratio of about 1.22.
- Calibration regression (xG >= 5): xG coefficient of about 1.03, R2 of about 0.78, stable with league and season fixed effects.
- Clusters: k = 2 by silhouette (0.29), weak separation; the main axis is chance creation (key passes, xA).
