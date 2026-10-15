# 13 - Part 2: goalkeepers. Do elite goalkeepers concede fewer goals than xGA?
# Uses team-season data from 08_download_team_data.R (goals conceded vs xGA).

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

understat <- readr::read_rds("data/processed/understat_clean.rds")
team_seasons <- readr::read_rds("data/processed/team_seasons.rds")

gk <- build_gk_seasons(understat, team_seasons) |>
  add_prior_skill()
cli::cli_bullets(c(
  "i" = "{nrow(gk)} team-seasons with a main goalkeeper ({dplyr::n_distinct(gk$id)} goalkeepers)",
  "i" = "{sum(!is.na(gk$elite))} seasons eligible for the elite test; {sum(gk$elite, na.rm = TRUE)} elite"
))

save_tab(
  describe_vars(gk, c("ga", "xga", "ga_xga", "goals_prevented", "xga_90")),
  "p2_gk_desc_stats", "Descriptive statistics of goalkeeper team-seasons"
)
save_tab(gk_group_summary(gk), "p2_gk_groups", "Elite vs other goalkeepers")

tests <- run_gk_tests(gk)
tests_short <- tests |>
  dplyr::mutate(
    hypothesis = c(
      "Elite: mean ga/xGA < 1 (t-test)",
      "Elite: median ga/xGA < 1 (Wilcoxon)",
      "Other: mean ga/xGA = 1 (t-test)",
      "Elite mean < other mean (Welch)",
      "Elite distribution < other (Wilcoxon)",
      "xGA per match: elite = other (Welch)"
    ),
    p_value = format.pval(p_value, digits = 3, eps = 1e-4)
  ) |>
  dplyr::select(hypothesis, statistic, p_value)
save_tab(tests, "p2_gk_tests", "Goalkeeper hypothesis tests", tab_tex = tests_short)

gk_models <- fit_gk_models(gk)
save_texreg(gk_models, "p2_gk_models", "Goalkeeper models (goals conceded vs xGA)")
cli::cli_verbatim(texreg::screenreg(gk_models))

# persistence of the goalkeeper ratio between seasons
gk_vars <- c("ga_xga", "goals_prevented_90", "xga_90")
gk_pairs <- make_lagged_pairs(gk, gk_vars)
save_tab(persistence_table(gk_pairs, gk_vars), "p2_gk_persistence",
         "Goalkeeper year-to-year persistence (same goalkeeper, seasons t and t+1)")
save_fig(plot_persistence(gk_pairs, "ga_xga", "Goals conceded / xGA"), "p2_gk_persistence_ga_xga")
save_fig(plot_gk_box(gk), "p2_gk_box")

# goalkeepers with the best and worst multi-season ratios (3+ seasons)
career <- gk |>
  dplyr::summarise(seasons = dplyr::n(), ga = sum(ga), xga = sum(xga),
                   ga_xga = sum(ga) / sum(xga), .by = c(id, player_name)) |>
  dplyr::filter(seasons >= 4)
save_tab(
  dplyr::bind_rows(
    career |> dplyr::arrange(ga_xga) |> dplyr::slice_head(n = 10) |> dplyr::mutate(type = "Lowest"),
    career |> dplyr::arrange(dplyr::desc(ga_xga)) |> dplyr::slice_head(n = 10) |> dplyr::mutate(type = "Highest")
  ),
  "p2_gk_career_extremes", "Goalkeepers with the lowest and highest ga/xGA (4+ seasons)"
)

readr::write_rds(gk, "data/processed/p2_goalkeepers.rds")
