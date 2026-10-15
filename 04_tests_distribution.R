# 04 - Normalidade, ajuste de distribuicoes, testes de hipotese e bootstrap

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

vars <- c("goals", "xg", "goals_xg", "npg_npxg", "goals_minus_xg", "shots_90", "xg_90")
fmt_p <- \(p) format.pval(p, digits = 3, eps = 1e-4)

for (tag in c("g10", "g20")) {
  cli::cli_h2("Base {tag}")
  base <- readr::read_rds(paste0("data/processed/forwards_", tag, ".rds"))
  x <- base$goals_xg

  # normalidade
  norm_tab <- purrr::map(vars, \(v) normality_tests(base[[v]], v)) |> dplyr::bind_rows()
  save_tab(norm_tab, paste0("normalidade_", tag), "Testes de normalidade (p-valores)",
           tab_tex = norm_tab |>
             dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), fmt_p)))

  # distribuicoes
  fits <- fit_distributions(x)
  save_tab(summarise_fits(fits), paste0("ajuste_dist_", tag),
           "Comparacao de distribuicoes ajustadas a gols/xG")
  save_tab(fit_parameters(fits), paste0("parametros_dist_", tag), "Parametros estimados")
  save_fit_plots(fits, x, tag)

  # testes de hipotese
  tests <- run_hypothesis_tests(base)
  save_tab(tests, paste0("testes_", tag), "Testes de hipoteses",
           tab_tex = tests |> dplyr::mutate(p_value = fmt_p(p_value)))
  save_tab(pairwise_leagues(base), paste0("pareado_ligas_", tag),
           "P-valores pareados entre ligas (Wilcoxon, Holm)")

  # bootstrap
  save_tab(bootstrap_ci(x), paste0("ic_bootstrap_", tag),
           "Intervalos de confianca bootstrap (95 por cento) para gols/xG")
}
