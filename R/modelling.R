# Multivariate models (fixed-effects regression) and clusters

# Regressions of goals/xG and goals, with league and season fixed effects
# and standard errors clustered by player
fit_ratio_models <- function(data) {
  list(
    "(1) goals/xG" = fixest::feols(goals_xg ~ xg, data = data, cluster = ~id),
    "(2) + shots/90, minutes" = fixest::feols(
      goals_xg ~ xg + shots_90 + time, data = data, cluster = ~id),
    "(3) league FE" = fixest::feols(
      goals_xg ~ xg + shots_90 + time | league, data = data, cluster = ~id),
    "(4) league + season FE" = fixest::feols(
      goals_xg ~ xg + shots_90 + time | league + season, data = data, cluster = ~id)
  )
}

fit_calibration_models <- function(data) {
  list(
    "(1) goals" = fixest::feols(goals ~ xg, data = data, cluster = ~id),
    "(2) league FE" = fixest::feols(goals ~ xg | league, data = data, cluster = ~id),
    "(3) league + season FE" = fixest::feols(
      goals ~ xg | league + season, data = data, cluster = ~id)
  )
}

tidy_models <- function(models) {
  purrr::map(models, broom::tidy) |>
    dplyr::bind_rows(.id = "model")
}

save_texreg <- function(models, name, caption) {
  texreg::texreg(models, file = file.path("tabs", paste0(name, ".tex")),
                 caption = caption, label = paste0("tab:", name),
                 booktabs = TRUE, dcolumn = FALSE, use.packages = FALSE)
  shrink_wide_tabular(file.path("tabs", paste0(name, ".tex")))
  texreg::htmlreg(models, file = file.path("tabs", paste0(name, ".html")),
                  caption = caption)
  readr::write_rds(tidy_models(models), file.path("tabs", paste0(name, ".rds")))
  cli::cli_alert_success("Model table saved: {.file tabs/{name}}")
}

# k-means on the finishing profile (standardised variables)
cluster_profile_vars <- c("goals_xg", "xg_90", "shots_90", "key_passes_90", "xa_90")

choose_k <- function(scaled, k_range = 2:6) {
  dist_mat <- stats::dist(scaled)
  purrr::map(k_range, \(k) {
    km <- stats::kmeans(scaled, centers = k, nstart = 25)
    sil <- cluster::silhouette(km$cluster, dist_mat)
    tibble::tibble(k = k, tot_withinss = km$tot.withinss,
                   avg_silhouette = mean(sil[, "sil_width"]))
  }) |>
    dplyr::bind_rows()
}

plot_clusters_pca <- function(data, scaled) {
  pca <- stats::prcomp(scaled)
  scores <- tibble::as_tibble(pca$x[, 1:2]) |>
    dplyr::mutate(cluster = data$cluster)
  var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
  ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2, color = cluster)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::labs(x = paste0("PC1 (", var_exp[1], "%)"),
                  y = paste0("PC2 (", var_exp[2], "%)"), color = "Cluster")
}
