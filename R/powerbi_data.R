# Datasets exported for the Power BI project (one csv per model table)

pbi_player_seasons <- function(indexed) {
  indexed |>
    dplyr::arrange(id, year) |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::select(
      row_id, id, player_name, season, year, team_title, league, goals, xg, shots, time,
      goals_xg, goals_minus_xg, shots_90, xg_90, xa_90, key_passes_90, xg_per_shot,
      index_equal, index_pca
    )
}

pbi_persistence_pairs <- function(indexed) {
  make_lagged_pairs(indexed, c("goals_xg", "xg_90")) |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::select(row_id, id, player_name, year, goals_xg_t, goals_xg_t1, xg_90_t, xg_90_t1)
}

pbi_bias_goals_cut <- function(cuts, sim) {
  cuts |>
    dplyr::filter(rule == "min_goals") |>
    dplyr::select(threshold, n, real_mean = mean) |>
    dplyr::left_join(
      sim |> dplyr::select(threshold = goals_min, simulated_mean = mean_ratio),
      by = "threshold"
    )
}

pbi_bias_xg_cut <- function(cuts) {
  cuts |>
    dplyr::filter(rule == "min_xg") |>
    dplyr::select(threshold, n, real_mean = mean)
}

pbi_pca_loadings_long <- function(loadings) {
  loadings |>
    tidyr::pivot_longer(-variable, names_to = "component", values_to = "loading")
}

pbi_career_index <- function(indexed, min_seasons = 3) {
  indexed |>
    dplyr::summarise(
      seasons = dplyr::n(), goals = sum(goals), xg = sum(xg), xg_90 = mean(xg_90),
      index_equal = mean(index_equal), index_pca = mean(index_pca),
      .by = c(id, player_name)
    ) |>
    dplyr::filter(seasons >= min_seasons) |>
    dplyr::mutate(goals_xg = goals / xg) |>
    dplyr::select(player_name, seasons, goals, xg, goals_xg, xg_90, index_equal, index_pca)
}

pbi_gk_seasons <- function(gk) {
  gk |>
    dplyr::arrange(id, year) |>
    dplyr::mutate(
      row_id = dplyr::row_number(),
      elite_group = dplyr::case_when(
        is.na(elite) ~ "Not eligible",
        elite ~ "Elite (out of sample)",
        TRUE ~ "Other"
      )
    ) |>
    dplyr::select(row_id, player_name, team_title, league, season, year, ga, xga, ga_xga,
                  goals_prevented, xga_90, prior_ratio, elite_group)
}

pbi_gk_career <- function(gk, min_seasons = 4) {
  gk |>
    dplyr::summarise(seasons = dplyr::n(), ga = sum(ga), xga = sum(xga),
                     .by = c(id, player_name)) |>
    dplyr::filter(seasons >= min_seasons) |>
    dplyr::mutate(ga_xga = ga / xga, goals_prevented = xga - ga) |>
    dplyr::select(player_name, seasons, ga, xga, ga_xga, goals_prevented)
}
