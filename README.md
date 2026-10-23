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

### Part 2 (multivariate evaluation)

| Script | What it does |
|---|---|
| `08_download_team_data.R` | team-season goals conceded and xGA from Understat (goalkeeper analysis) |
| `09_player_seasons.R` | merges transfers by player id + season; main sample = forwards with xG >= 5 |
| `10_regression_extended.R` | goals ~ xG + shots + minutes + position + league/season FE, Poisson check, diagnostics |
| `11_pca_index.R` | PCA of finishing/creation metrics and two composite offensive indices |
| `12_persistence.R` | year-to-year persistence (skill vs luck) and next-season prediction |
| `13_goalkeepers.R` | do elite goalkeepers concede fewer goals than xGA? |

| `15_build_powerbi.R` | builds the Power BI project `powerbi/sports_stats.pbip` (5 pages) from the outputs |

The Power BI project is generated as text (TMDL + PBIR), so it is versioned and reproducible. Open `powerbi/sports_stats.pbip` in Power BI Desktop and click Refresh. If the project folder moves, update the `DataFolder` parameter (Transform data > Edit parameters). The original `pbi_gols_xg.pbix` is kept untouched.

The LaTeX write-up is `report/part2.tex` (upload the whole folder to Overleaf).

Reusable functions live in `R/`. Outputs go to `tabs/` and `figs/`. Old code is kept in `legacy/` (original Portuguese version), and the original course materials are in `docs/`.

## Main results

- g10 dataset (n = 1139): mean goals/xG = 1.156, median 1.122, skewness 1.17.
- With a **minimum-xG** cutoff (which does not select on the outcome) the mean drops to about 0.99 and is no longer different from 1 (xG >= 5: p = 0.16). The "1.16" comes from selection bias.
- Simulation with goals ~ Poisson(xG), i.e. no skill at all: the 10-goal cutoff alone already produces a mean ratio of about 1.22.
- Calibration regression (xG >= 5): xG coefficient of about 1.03, R2 of about 0.78, stable with league and season fixed effects.
- Clusters: k = 2 by silhouette (0.29), weak separation; the main axis is chance creation (key passes, xA).

### Part 2 results

- xG is well calibrated for forwards (slope 1.03, R2 0.78); extra shots add little (0.02 goals per shot).
- Persistence: goals/xG has a year-to-year correlation of only 0.09, against 0.64 for xG per 90. Most single-season finishing is noise.
- PCA: three components explain 78% of the variance; finishing is a separate dimension from creation and volume.
- Composite indices describe profiles well but predict next-season goals worse (R2 0.14-0.17) than xG per 90 alone (0.28).
- Goalkeepers (out-of-sample elite definition): elite goalkeepers concede 3.8% fewer goals than xGA (ratio 0.962, p < 0.001); other goalkeepers are at 1.0.
