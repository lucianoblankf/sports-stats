# Testes de normalidade, ajuste de distribuicoes e testes de hipotese

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

# Uma linha da tabela de testes: H0, H1 e resultado de um objeto htest
htest_row <- function(h0, h1, res) {
  tibble::tibble(
    h0 = h0, h1 = h1, test = res$method,
    statistic = unname(res$statistic), p_value = res$p.value
  )
}

run_hypothesis_tests <- function(data) {
  tests <- dplyr::bind_rows(
    htest_row("Media de gols/xG igual a 1", "Media diferente de 1",
              stats::t.test(data$goals_xg, mu = 1)),
    htest_row("Mediana de gols/xG igual a 1", "Mediana diferente de 1",
              stats::wilcox.test(data$goals_xg, mu = 1)),
    htest_row("Media de (gols menos xG) igual a 0", "Media diferente de 0",
              stats::t.test(data$goals, data$xg, paired = TRUE)),
    htest_row("Mediana de (gols menos xG) igual a 0", "Mediana diferente de 0",
              stats::wilcox.test(data$goals, data$xg, paired = TRUE)),
    htest_row("Proporcao de atacantes com gols acima do xG igual a 0,5",
              "Proporcao diferente de 0,5",
              stats::binom.test(sum(data$goals > data$xg), nrow(data), p = 0.5)),
    htest_row("Correlacao de Pearson entre xG e gols igual a 0", "Correlacao diferente de 0",
              stats::cor.test(data$xg, data$goals)),
    htest_row("Correlacao de Spearman entre xG e gols igual a 0", "Correlacao diferente de 0",
              stats::cor.test(data$xg, data$goals, method = "spearman", exact = FALSE)),
    htest_row("Medias de gols/xG iguais entre as ligas", "Ao menos uma liga difere",
              stats::oneway.test(goals_xg ~ league, data = data)),
    htest_row("Distribuicao de gols/xG igual entre as ligas", "Ao menos uma liga difere",
              stats::kruskal.test(goals_xg ~ league, data = data)),
    htest_row("Variancias de gols/xG iguais entre as ligas", "Ao menos uma liga difere",
              stats::fligner.test(goals_xg ~ league, data = data)),
    htest_row("Distribuicao de gols/xG igual entre as temporadas",
              "Ao menos uma temporada difere",
              stats::kruskal.test(goals_xg ~ season, data = data))
  )

  # inclinacao da regressao gols ~ xG igual a 1 (xG "calibrado")
  cf <- stats::coef(summary(stats::lm(goals ~ xg, data = data)))
  dof <- stats::df.residual(stats::lm(goals ~ xg, data = data))
  t_b1 <- (cf["xg", "Estimate"] - 1) / cf["xg", "Std. Error"]
  tests <- dplyr::bind_rows(tests, tibble::tibble(
    h0 = "Coeficiente de xG na regressao de gols igual a 1",
    h1 = "Coeficiente diferente de 1",
    test = "Teste t para o coeficiente",
    statistic = unname(t_b1), p_value = unname(2 * stats::pt(-abs(t_b1), df = dof))
  ))

  # 10-19 gols vs 20+ gols (so faz sentido quando ha os dois grupos)
  if (dplyr::n_distinct(data$group_goals) == 2) {
    tests <- dplyr::bind_rows(
      tests,
      htest_row("Media de gols/xG igual entre os grupos 10-19 e 20+ gols", "Medias diferentes",
                stats::t.test(goals_xg ~ group_goals, data = data)),
      htest_row("Distribuicao de gols/xG igual entre os grupos 10-19 e 20+ gols",
                "Distribuicoes diferentes",
                stats::wilcox.test(goals_xg ~ group_goals, data = data))
    )
  }

  # sensibilidade: um valor por jogador (media das temporadas)
  per_player <- data |>
    dplyr::summarise(goals_xg = mean(goals_xg), .by = id)
  dplyr::bind_rows(
    tests,
    htest_row("Media de gols/xG igual a 1 (um valor por jogador)", "Media diferente de 1",
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
