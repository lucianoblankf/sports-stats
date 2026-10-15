# 09 - Part 2: one row per player-season (transfers merged) and main sample (xG >= 5)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()

understat <- readr::read_rds("data/processed/understat_clean.rds")

merged <- merge_player_seasons(understat)
n_multi <- sum(merged$n_clubs > 1)
cli::cli_bullets(c(
  "i" = "{nrow(understat)} club-level rows -> {nrow(merged)} player-seasons",
  "i" = "{n_multi} player-seasons with more than one club (merged)"
))

forwards_xg5 <- build_forwards_merged(understat, xg_min = 5) |>
  add_position_flags()
forwards_g10 <- build_forwards_merged(understat, goals_min = 10) |>
  add_position_flags()

cli::cli_bullets(c(
  "v" = "Main sample (xG >= 5): {nrow(forwards_xg5)} player-seasons, {dplyr::n_distinct(forwards_xg5$id)} players",
  "v" = "Robustness sample (goals >= 10): {nrow(forwards_g10)} player-seasons"
))

save_tab(
  forwards_xg5 |> dplyr::count(league, name = "n") |> dplyr::mutate(pct = round(100 * n / sum(n), 1)),
  "p2_sample_by_league", "Part 2 sample (xG $\\geq$ 5) by league"
)
save_tab(
  describe_vars(forwards_xg5, c("goals", "xg", "shots", "time", "goals_xg", "shots_90",
                                "xg_90", "xg_per_shot", "xa_90", "key_passes_90")),
  "p2_desc_stats", "Descriptive statistics, Part 2 sample (xG $\\geq$ 5)"
)

readr::write_rds(forwards_xg5, "data/processed/p2_forwards_xg5.rds")
readr::write_rds(forwards_g10, "data/processed/p2_forwards_g10.rds")
