# 11 - Part 2: PCA of finishing/creation metrics and composite offensive index

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

base <- readr::read_rds("data/processed/p2_forwards_xg5.rds") |>
  tidyr::drop_na(dplyr::all_of(pca_vars))

# correlation matrix of the metrics
cor_tab <- stats::cor(base[pca_vars]) |>
  tibble::as_tibble(rownames = "variable")
save_tab(cor_tab, "p2_pca_correlations", "Correlation matrix of the PCA variables")

# PCA
pca <- run_pca(base)
var_tab <- pca_variance_table(pca)
save_tab(var_tab, "p2_pca_variance", "Variance explained by the principal components")
save_tab(pca_loadings_table(pca), "p2_pca_loadings", "PCA loadings (first four components)")
save_fig(plot_scree(var_tab), "p2_pca_scree")
save_fig(plot_biplot(pca, base), "p2_pca_biplot", width = 8, height = 6)

# composite index
indexed <- build_composite_index(base, pca)
save_tab(top_bottom_index(indexed, "index_equal"), "p2_index_equal_ranking",
         "Highest and lowest equal-weight composite index")
save_tab(top_bottom_index(indexed, "index_pca"), "p2_index_pca_ranking",
         "Highest and lowest PCA-weighted index")
save_tab(
  tibble::tibble(
    comparison = "Equal-weight index vs PCA-weighted index",
    pearson_r = stats::cor(indexed$index_equal, indexed$index_pca),
    spearman_rho = stats::cor(indexed$index_equal, indexed$index_pca, method = "spearman")
  ),
  "p2_index_agreement", "Agreement between the two composite indices"
)

# career-level ranking: players with at least 3 seasons in the sample
career <- indexed |>
  dplyr::summarise(
    seasons = dplyr::n(), goals = sum(goals), xg = sum(xg),
    index_equal = mean(index_equal), index_pca = mean(index_pca),
    .by = c(id, player_name)
  ) |>
  dplyr::filter(seasons >= 3) |>
  dplyr::mutate(goals_xg = goals / xg) |>
  dplyr::arrange(dplyr::desc(index_equal)) |>
  dplyr::slice_head(n = 15)
save_tab(career, "p2_index_career_top15", "Top 15 forwards by mean composite index (3+ seasons)")

readr::write_rds(indexed, "data/processed/p2_forwards_indexed.rds")
