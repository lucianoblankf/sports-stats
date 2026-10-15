# 12 - Part 2: year-to-year persistence (skill vs luck) and out-of-sample index validation

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

indexed <- readr::read_rds("data/processed/p2_forwards_indexed.rds")

vars <- c("goals_xg", "npg_npxg", "xg_90", "shots_90", "xg_per_shot", "xa_90",
          "key_passes_90", "goals_90")
pairs <- make_lagged_pairs(indexed |> tidyr::drop_na(dplyr::all_of(vars)), vars)
cli::cli_alert_info("{nrow(pairs)} consecutive player-season pairs ({dplyr::n_distinct(pairs$id)} players)")

pers <- persistence_table(pairs, vars)
save_tab(pers, "p2_persistence", "Year-to-year persistence of each metric (season t vs t+1)")
save_fig(plot_persistence(pairs, "goals_xg", "Goals/xG"), "p2_persistence_goals_xg")
save_fig(plot_persistence(pairs, "xg_90", "xG per 90"), "p2_persistence_xg_90")

# regression of next-season goals per 90 on current components (what predicts scoring?)
pred_pairs <- make_lagged_pairs(
  indexed, c("goals_90", "goals_xg", "xg_90", "shots_90", "xa_90", "index_equal", "index_pca")
) |>
  tidyr::drop_na()

pred_models <- list(
  "(1) goals/xG" = fixest::feols(goals_90_t1 ~ goals_xg_t, data = pred_pairs, cluster = ~id),
  "(2) xG/90" = fixest::feols(goals_90_t1 ~ xg_90_t, data = pred_pairs, cluster = ~id),
  "(3) goals/xG + xG/90" = fixest::feols(
    goals_90_t1 ~ goals_xg_t + xg_90_t, data = pred_pairs, cluster = ~id),
  "(4) + shots/90, xA/90" = fixest::feols(
    goals_90_t1 ~ goals_xg_t + xg_90_t + shots_90_t + xa_90_t, data = pred_pairs, cluster = ~id),
  "(5) equal-weight index" = fixest::feols(
    goals_90_t1 ~ index_equal_t, data = pred_pairs, cluster = ~id),
  "(6) PCA index" = fixest::feols(
    goals_90_t1 ~ index_pca_t, data = pred_pairs, cluster = ~id)
)
save_texreg(pred_models, "p2_reg_next_season_goals",
            "Predicting next-season goals per 90 with current-season metrics")

fit_stats <- tibble::tibble(
  model = names(pred_models),
  r2 = purrr::map_dbl(pred_models, \(m) unname(fixest::r2(m, "r2"))),
  n = purrr::map_dbl(pred_models, stats::nobs)
)
save_tab(fit_stats, "p2_prediction_fit", "Out-of-season fit of the prediction models")
