# 03 - Descriptive statistics and figures (for the g10 and g20 datasets)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()

vars <- c("goals", "xg", "goals_xg", "npg_npxg", "goals_minus_xg", "shots_90", "xg_90")

for (tag in c("g10", "g20")) {
  cli::cli_h2("Dataset {tag}")
  base <- readr::read_rds(paste0("data/processed/forwards_", tag, ".rds"))

  save_tab(describe_vars(base, vars), paste0("desc_stats_", tag), "Descriptive statistics")
  save_tab(
    dplyr::count(base, league, name = "n") |> dplyr::mutate(pct = round(100 * n / sum(n), 1)),
    paste0("n_by_league_", tag), "Observations by league"
  )
  save_tab(dplyr::count(base, season, name = "n"),
           paste0("n_by_season_", tag), "Observations by season")
  save_tab(extreme_players(base), paste0("extreme_players_", tag),
           "Highest and lowest goals/xG ratios")
  save_tab(summarise_by_league(base), paste0("ratio_by_league_", tag), "Goals/xG by league")

  save_fig(plot_hist_ratio(base), paste0("hist_goals_xg_", tag))
  save_fig(plot_qq_ratio(base), paste0("qq_goals_xg_", tag))
  save_fig(plot_box_ratio(base, "league"), paste0("box_league_", tag))
  save_fig(plot_box_ratio(base, "season", rotate = TRUE), paste0("box_season_", tag))
  save_fig(plot_scatter_xg_goals(base), paste0("scatter_xg_goals_", tag))
}
