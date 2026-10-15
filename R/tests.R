# Normality tests, distribution fitting and hypothesis tests

normality_tests <- function(x, name) {
  tibble::tibble(
    variable = name,
    n = length(x),
    shapiro_p = if (length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_,
    anderson_p = nortest::ad.test(x)$p.value,
    lilliefors_p = nortest::lillie.test(x)$p.value,
    jarque_bera_p = moments::jarque.test(x)$p.value
  )
}

fit_distributions <- function(x) {
  list(
    normal = fitdistrplus::fitdist(x, "norm"),
    lognormal = fitdistrplus::fitdist(x, "lnorm"),
    gamma = fitdistrplus::fitdist(x, "gamma"),
    weibull = fitdistrplus::fitdist(x, "weibull")
  )
}

summarise_fits <- function(fits) {
  gof <- fitdistrplus::gofstat(fits, fitnames = names(fits))
  tibble::tibble(
    distribution = names(fits),
    aic = purrr::map_dbl(fits, "aic"),
    bic = purrr::map_dbl(fits, "bic"),
    ks = unname(gof$ks),
    ad = unname(gof$ad)
  ) |>
    dplyr::arrange(aic)
}

fit_parameters <- function(fits) {
  purrr::map(fits, \(a) tibble::tibble(parameter = names(a$estimate),
                                      value = unname(a$estimate))) |>
    dplyr::bind_rows(.id = "distribution")
}

# One row of the test table: H0, H1 and the result of an htest object
htest_row <- function(h0, h1, res) {
  tibble::tibble(
    h0 = h0, h1 = h1, test = res$method,
    statistic = unname(res$statistic), p_value = res$p.value
  )
}

run_hypothesis_tests <- function(data) {
  tests <- dplyr::bind_rows(
    htest_row("Mean goals/xG equals 1", "Mean differs from 1",
              stats::t.test(data$goals_xg, mu = 1)),
    htest_row("Median goals/xG equals 1", "Median differs from 1",
              stats::wilcox.test(data$goals_xg, mu = 1)),
    htest_row("Mean (goals minus xG) equals 0", "Mean differs from 0",
              stats::t.test(data$goals, data$xg, paired = TRUE)),
    htest_row("Median (goals minus xG) equals 0", "Median differs from 0",
              stats::wilcox.test(data$goals, data$xg, paired = TRUE)),
    htest_row("Share of forwards with goals above xG equals 0.5",
              "Share differs from 0.5",
              stats::binom.test(sum(data$goals > data$xg), nrow(data), p = 0.5)),
    htest_row("Pearson correlation between xG and goals equals 0",
              "Correlation differs from 0",
              stats::cor.test(data$xg, data$goals)),
    htest_row("Spearman correlation between xG and goals equals 0",
              "Correlation differs from 0",
              stats::cor.test(data$xg, data$goals, method = "spearman", exact = FALSE)),
    htest_row("Mean goals/xG is equal across leagues", "At least one league differs",
              stats::oneway.test(goals_xg ~ league, data = data)),
    htest_row("Goals/xG distribution is equal across leagues",
              "At least one league differs",
              stats::kruskal.test(goals_xg ~ league, data = data)),
    htest_row("Goals/xG variances are equal across leagues",
              "At least one league differs",
              stats::fligner.test(goals_xg ~ league, data = data)),
    htest_row("Goals/xG distribution is equal across seasons",
              "At least one season differs",
              stats::kruskal.test(goals_xg ~ season, data = data))
  )

  # slope of the goals ~ xG regression equals 1 (calibrated xG)
  calib_model <- stats::lm(goals ~ xg, data = data)
  cf <- stats::coef(summary(calib_model))
  t_b1 <- (cf["xg", "Estimate"] - 1) / cf["xg", "Std. Error"]
  tests <- dplyr::bind_rows(tests, tibble::tibble(
    h0 = "xG coefficient in the goals regression equals 1",
    h1 = "Coefficient differs from 1",
    test = "t-test for the coefficient",
    statistic = unname(t_b1),
    p_value = unname(2 * stats::pt(-abs(t_b1), df = stats::df.residual(calib_model)))
  ))

  # 10-19 goals vs 20+ goals (only meaningful when both groups exist)
  if (dplyr::n_distinct(data$group_goals) == 2) {
    tests <- dplyr::bind_rows(
      tests,
      htest_row("Mean goals/xG is equal between the 10-19 and 20+ goals groups",
                "Means differ",
                stats::t.test(goals_xg ~ group_goals, data = data)),
      htest_row("Goals/xG distribution is equal between the 10-19 and 20+ goals groups",
                "Distributions differ",
                stats::wilcox.test(goals_xg ~ group_goals, data = data))
    )
  }

  # sensitivity check: one value per player (mean across seasons)
  per_player <- data |>
    dplyr::summarise(goals_xg = mean(goals_xg), .by = id)
  dplyr::bind_rows(
    tests,
    htest_row("Mean goals/xG equals 1 (one value per player)", "Mean differs from 1",
              stats::t.test(per_player$goals_xg, mu = 1))
  )
}

pairwise_leagues <- function(data) {
  pw <- stats::pairwise.wilcox.test(data$goals_xg, data$league, p.adjust.method = "holm")
  as.data.frame(pw$p.value) |>
    tibble::rownames_to_column("league")
}

bootstrap_ci <- function(x, n_boot = 5000, level = 0.95) {
  alpha <- (1 - level) / 2
  boot_mean <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  boot_median <- replicate(n_boot, stats::median(sample(x, replace = TRUE)))
  tibble::tibble(
    statistic = c("mean", "median"),
    estimate = c(mean(x), stats::median(x)),
    ci_low = unname(c(stats::quantile(boot_mean, alpha), stats::quantile(boot_median, alpha))),
    ci_high = unname(c(stats::quantile(boot_mean, 1 - alpha), stats::quantile(boot_median, 1 - alpha)))
  )
}
