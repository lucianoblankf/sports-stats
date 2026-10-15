# Part 2: extended regressions, PCA, composite index and persistence

add_position_flags <- function(data) {
  data |>
    dplyr::mutate(also_midfield = as.integer(stringr::str_detect(position, "M")))
}

# Variance inflation factors for a set of regressors
vif_table <- function(data, vars) {
  purrr::map(vars, \(v) {
    others <- setdiff(vars, v)
    r2 <- summary(stats::lm(stats::reformulate(others, v), data = data))$r.squared
    tibble::tibble(variable = v, vif = 1 / (1 - r2))
  }) |>
    dplyr::bind_rows()
}

# Linear specifications of goals on xG, shots, minutes, position, league and season
fit_extended_models <- function(data) {
  list(
    "(1) xG" = fixest::feols(goals ~ xg, data = data, cluster = ~id),
    "(2) + shots" = fixest::feols(goals ~ xg + shots, data = data, cluster = ~id),
    "(3) + minutes, midfield" = fixest::feols(
      goals ~ xg + shots + time + also_midfield, data = data, cluster = ~id),
    "(4) league FE" = fixest::feols(
      goals ~ xg + shots + time + also_midfield | league, data = data, cluster = ~id),
    "(5) league + season FE" = fixest::feols(
      goals ~ xg + shots + time + also_midfield | league + season,
      data = data, cluster = ~id)
  )
}

# Poisson (count data) alternative with the same controls
fit_poisson_models <- function(data) {
  list(
    "(1) Poisson" = fixest::fepois(
      goals ~ log(xg) + log(shots) + also_midfield, data = data, cluster = ~id),
    "(2) Poisson, league + season FE" = fixest::fepois(
      goals ~ log(xg) + log(shots) + also_midfield | league + season,
      data = data, cluster = ~id)
  )
}

# Residual diagnostics for the linear model with FE (estimated with lm for plotting)
fit_diagnostic_lm <- function(data) {
  stats::lm(goals ~ xg + shots + time + also_midfield + league + season, data = data)
}

# White-type heteroskedasticity test: n * R2 of resid^2 on fitted and fitted^2
white_test <- function(model) {
  res2 <- stats::resid(model)^2
  fit <- stats::fitted(model)
  aux <- summary(stats::lm(res2 ~ fit + I(fit^2)))$r.squared
  stat <- length(res2) * aux
  tibble::tibble(test = "White-type (fitted values)", statistic = stat,
                 df = 2, p_value = stats::pchisq(stat, df = 2, lower.tail = FALSE))
}

plot_residual_diagnostics <- function(model) {
  d <- tibble::tibble(fitted = stats::fitted(model), resid = stats::resid(model))
  p1 <- ggplot2::ggplot(d, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "black") +
    ggplot2::labs(x = "Fitted goals", y = "Residual", title = "Residuals vs fitted")
  p2 <- ggplot2::ggplot(d, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq(alpha = 0.5) + ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(x = "Theoretical quantiles", y = "Residual quantiles",
                  title = "Residual QQ-plot")
  list(resid_fitted = p1, resid_qq = p2)
}

# ---- PCA ------------------------------------------------------------------

pca_vars <- c("goals_xg", "xg_90", "shots_90", "xg_per_shot", "xa_90",
              "key_passes_90", "xg_buildup_90")

run_pca <- function(data, vars = pca_vars) {
  stats::prcomp(data[vars], center = TRUE, scale. = TRUE)
}

pca_variance_table <- function(pca) {
  tibble::tibble(
    component = paste0("PC", seq_along(pca$sdev)),
    eigenvalue = pca$sdev^2,
    variance_pct = 100 * pca$sdev^2 / sum(pca$sdev^2),
    cumulative_pct = cumsum(100 * pca$sdev^2 / sum(pca$sdev^2))
  )
}

pca_loadings_table <- function(pca, n_components = 4) {
  tibble::as_tibble(pca$rotation[, seq_len(n_components)], rownames = "variable")
}

plot_scree <- function(var_tab) {
  ggplot2::ggplot(var_tab, ggplot2::aes(seq_along(component), eigenvalue)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = seq_along(var_tab$component)) +
    ggplot2::labs(x = "Component", y = "Eigenvalue",
                  subtitle = "Dashed line: Kaiser criterion (eigenvalue = 1)")
}

plot_biplot <- function(pca, data, color_var = "league") {
  scores <- tibble::as_tibble(pca$x[, 1:2]) |>
    dplyr::mutate(group = data[[color_var]])
  arrows <- tibble::as_tibble(pca$rotation[, 1:2], rownames = "variable") |>
    dplyr::mutate(PC1 = PC1 * 4, PC2 = PC2 * 4)
  var_exp <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
  ggplot2::ggplot() +
    ggplot2::geom_point(data = scores, ggplot2::aes(PC1, PC2, color = group), alpha = 0.35) +
    ggplot2::geom_segment(data = arrows, ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
                          arrow = grid::arrow(length = grid::unit(0.2, "cm")), color = "black") +
    ggplot2::geom_text(data = arrows, ggplot2::aes(PC1, PC2, label = variable),
                       vjust = -0.5, size = 3) +
    ggplot2::labs(x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"),
                  color = NULL) +
    ggplot2::theme(legend.position = "bottom")
}

# ---- Composite offensive index ---------------------------------------------

zscore <- function(x) as.numeric(scale(x))

# Two composite indices:
# - index_equal: equal-weight average of three dimension z-scores (finishing,
#   chance volume, creation), a theory-driven index;
# - index_pca: the first `n_components` principal components weighted by their share
#   of variance, each sign-aligned to correlate positively with index_equal
#   (principal component signs are arbitrary).
build_composite_index <- function(data, pca, n_components = 3) {
  data <- data |>
    dplyr::mutate(
      z_finishing = zscore(goals_xg),
      z_volume = (zscore(xg_90) + zscore(shots_90)) / 2,
      z_creation = (zscore(xa_90) + zscore(key_passes_90)) / 2,
      index_equal = (z_finishing + z_volume + z_creation) / 3
    )
  weights <- pca$sdev[seq_len(n_components)]^2 / sum(pca$sdev[seq_len(n_components)]^2)
  signs <- sign(stats::cor(pca$x[, seq_len(n_components)], data$index_equal))
  data |>
    dplyr::mutate(index_pca = as.numeric(pca$x[, seq_len(n_components)] %*% (weights * signs)))
}

top_bottom_index <- function(data, index_var, n = 10) {
  cols <- c("player_name", "season", "team_title", "league", "goals", "xg", "goals_xg",
            index_var)
  dplyr::bind_rows(
    data |> dplyr::arrange(dplyr::desc(.data[[index_var]])) |>
      dplyr::slice_head(n = n) |> dplyr::mutate(type = "Top"),
    data |> dplyr::arrange(.data[[index_var]]) |>
      dplyr::slice_head(n = n) |> dplyr::mutate(type = "Bottom")
  ) |>
    dplyr::select(type, dplyr::all_of(cols))
}

# ---- Year-to-year persistence ------------------------------------------------

# Pairs each player-season with the same player's following season
make_lagged_pairs <- function(data, vars) {
  current <- data |>
    dplyr::select(id, year, player_name, dplyr::all_of(vars))
  following <- data |>
    dplyr::select(id, year, dplyr::all_of(vars)) |>
    dplyr::mutate(year = year - 1)
  dplyr::inner_join(current, following, by = c("id", "year"), suffix = c("_t", "_t1"))
}

persistence_table <- function(pairs, vars) {
  purrr::map(vars, \(v) {
    x <- pairs[[paste0(v, "_t")]]
    y <- pairs[[paste0(v, "_t1")]]
    fit <- stats::lm(y ~ x)
    ct <- stats::cor.test(x, y)
    tibble::tibble(
      variable = v,
      n_pairs = length(x),
      pearson_r = unname(ct$estimate),
      r_ci_low = ct$conf.int[1],
      r_ci_high = ct$conf.int[2],
      spearman_rho = stats::cor(x, y, method = "spearman"),
      slope = unname(stats::coef(fit)[2]),
      slope_se = summary(fit)$coefficients[2, 2]
    )
  }) |>
    dplyr::bind_rows()
}

plot_persistence <- function(pairs, var, label) {
  ggplot2::ggplot(pairs, ggplot2::aes(.data[[paste0(var, "_t")]], .data[[paste0(var, "_t1")]])) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "black") +
    ggplot2::labs(x = paste(label, "(season t)"), y = paste(label, "(season t+1)"))
}
