# 04 - Normality, distribution fitting, hypothesis tests and bootstrap

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

vars <- c("goals", "xg", "goals_xg", "npg_npxg", "goals_minus_xg", "shots_90", "xg_90")
fmt_p <- \(p) format.pval(p, digits = 3, eps = 1e-4)

for (tag in c("g10", "g20")) {
  cli::cli_h2("Dataset {tag}")
  base <- readr::read_rds(paste0("data/processed/forwards_", tag, ".rds"))
  x <- base$goals_xg

  # normality
  norm_tab <- purrr::map(vars, \(v) normality_tests(base[[v]], v)) |> dplyr::bind_rows()
  save_tab(norm_tab, paste0("normality_", tag), "Normality tests (p-values)",
           tab_tex = norm_tab |>
             dplyr::mutate(dplyr::across(dplyr::ends_with("_p"), fmt_p)))

  # distributions
  fits <- fit_distributions(x)
  save_tab(summarise_fits(fits), paste0("dist_fit_", tag),
           "Comparison of distributions fitted to goals/xG")
  save_tab(fit_parameters(fits), paste0("dist_params_", tag), "Estimated parameters")
  save_fit_plots(fits, x, tag)

  # hypothesis tests
  tests <- run_hypothesis_tests(base)
  save_tab(tests, paste0("hypothesis_tests_", tag), "Hypothesis tests",
           tab_tex = tests |> dplyr::mutate(p_value = fmt_p(p_value)))
  save_tab(pairwise_leagues(base), paste0("pairwise_leagues_", tag),
           "Pairwise p-values between leagues (Wilcoxon, Holm)")

  # bootstrap
  save_tab(bootstrap_ci(x), paste0("bootstrap_ci_", tag),
           "Bootstrap confidence intervals (95 percent) for goals/xG")
}
