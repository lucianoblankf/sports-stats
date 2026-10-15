# 07 - Finishing profiles via k-means (sample xG >= 5)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

understat <- readr::read_rds("data/processed/understat_clean.rds")
base_xg <- build_forwards(understat, xg_min = 5) |>
  tidyr::drop_na(dplyr::all_of(cluster_profile_vars))

scaled <- base_xg |>
  dplyr::select(dplyr::all_of(cluster_profile_vars)) |>
  scale()

k_tab <- choose_k(scaled)
save_tab(k_tab, "cluster_choice_k", "Choice of the number of clusters (mean silhouette)")
best_k <- k_tab$k[which.max(k_tab$avg_silhouette)]
cli::cli_alert_info("k chosen by mean silhouette: {best_k}")

km <- stats::kmeans(scaled, centers = best_k, nstart = 25)
base_xg <- base_xg |>
  dplyr::mutate(cluster = factor(km$cluster))

profile <- base_xg |>
  dplyr::summarise(
    n = dplyr::n(),
    dplyr::across(dplyr::all_of(c("goals", "xg", cluster_profile_vars)), mean),
    .by = cluster
  ) |>
  dplyr::arrange(cluster)
save_tab(profile, "cluster_profiles", "Mean profile of forward clusters")
save_fig(plot_clusters_pca(base_xg, scaled), "cluster_pca")
readr::write_rds(base_xg, "data/processed/forwards_xgmin5_clusters.rds")
