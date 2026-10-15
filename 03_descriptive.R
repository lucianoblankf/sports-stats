# 03 - Estatisticas descritivas e figuras (para as bases g10 e g20)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()

vars <- c("goals", "xg", "goals_xg", "npg_npxg", "goals_minus_xg", "shots_90", "xg_90")

for (tag in c("g10", "g20")) {
  cli::cli_h2("Base {tag}")
  base <- readr::read_rds(paste0("data/processed/forwards_", tag, ".rds"))

  save_tab(describe_vars(base, vars), paste0("desc_", tag), "Estatisticas descritivas")
  save_tab(
    dplyr::count(base, league, name = "n") |> dplyr::mutate(pct = round(100 * n / sum(n), 1)),
    paste0("n_liga_", tag), "Observacoes por liga"
  )
  save_tab(dplyr::count(base, season, name = "n"),
           paste0("n_temporada_", tag), "Observacoes por temporada")
  save_tab(extreme_players(base), paste0("extremos_", tag), "Maiores e menores razoes gols/xG")
  save_tab(summarise_by_league(base), paste0("gols_xg_liga_", tag), "Gols/xG por liga")

  save_fig(plot_hist_ratio(base), paste0("hist_goals_xg_", tag))
  save_fig(plot_qq_ratio(base), paste0("qq_goals_xg_", tag))
  save_fig(plot_box_ratio(base, "league"), paste0("box_liga_", tag))
  save_fig(plot_box_ratio(base, "season", rotate = TRUE), paste0("box_temporada_", tag))
  save_fig(plot_scatter_xg_goals(base), paste0("dispersao_xg_gols_", tag))
}
