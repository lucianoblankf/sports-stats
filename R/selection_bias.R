# Vies de selecao: efeito do corte (gols minimos vs xG minimo) sobre gols/xG

# Resume gols/xG sob um corte, com teste t de media = 1
summarise_cut <- function(data, rule, threshold) {
  tibble::tibble(
    rule = rule,
    threshold = threshold,
    n = nrow(data),
    mean = mean(data$goals_xg),
    median = stats::median(data$goals_xg),
    sd = stats::sd(data$goals_xg),
    p_value_mean_1 = stats::t.test(data$goals_xg, mu = 1)$p.value
  )
}

# Compara cortes por gols minimos (seleciona no resultado) e por xG minimo
compare_cuts <- function(raw_data, goals_cuts, xg_cuts) {
  by_goals <- purrr::map(goals_cuts, \(g) {
    summarise_cut(build_forwards(raw_data, goals_min = g), "gols_min", g)
  })
  by_xg <- purrr::map(xg_cuts, \(x) {
    summarise_cut(build_forwards(raw_data, xg_min = x), "xg_min", x)
  })
  dplyr::bind_rows(by_goals, by_xg)
}

# Simulacao: se todos finalizassem exatamente como o xG preve (gols ~ Poisson(xG)),
# o corte por gols minimos ainda produz razao media acima de 1?
simulate_selection <- function(raw_data, goals_cuts, n_sim = 500) {
  base <- build_forwards(raw_data, xg_min = 1)
  purrr::map(goals_cuts, \(g) {
    sims <- purrr::map(seq_len(n_sim), \(i) {
      sim_goals <- stats::rpois(nrow(base), base$xg)
      keep <- sim_goals >= g
      tibble::tibble(n = sum(keep), mean_ratio = mean(sim_goals[keep] / base$xg[keep]))
    }) |>
      dplyr::bind_rows()
    tibble::tibble(
      goals_min = g,
      mean_n = mean(sims$n),
      mean_ratio = mean(sims$mean_ratio, na.rm = TRUE)
    )
  }) |>
    dplyr::bind_rows()
}

plot_cut_comparison <- function(cuts) {
  ggplot2::ggplot(cuts, ggplot2::aes(threshold, mean, color = rule)) +
    ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(size = n)) +
    ggplot2::facet_wrap(~rule, scales = "free_x") +
    ggplot2::labs(x = "Valor do corte", y = "Media de gols/xG", size = "N",
                  color = NULL) +
    ggplot2::theme(legend.position = "bottom")
}
