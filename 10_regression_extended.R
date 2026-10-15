# 10 - Part 2: extended regressions (goals ~ xG + shots + position + league + season)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

base <- readr::read_rds("data/processed/p2_forwards_xg5.rds")
robust <- readr::read_rds("data/processed/p2_forwards_g10.rds")

# multicollinearity
save_tab(vif_table(base, c("xg", "shots", "time", "also_midfield")),
         "p2_vif", "Variance inflation factors")

# linear models
lin_models <- fit_extended_models(base)
save_texreg(lin_models, "p2_reg_goals_xg5",
            "Goals on xG, shots, minutes and position (forwards with xG $\\geq$ 5)")
cli::cli_verbatim(texreg::screenreg(lin_models))

# count-data alternative
pois_models <- fit_poisson_models(base)
save_texreg(pois_models, "p2_reg_poisson_xg5",
            "Poisson regressions of goals (forwards with xG $\\geq$ 5)")

# same specification on the goals >= 10 sample (selection-bias comparison)
save_texreg(fit_extended_models(robust), "p2_reg_goals_g10",
            "Goals on xG, shots, minutes and position (goals $\\geq$ 10 sample)")

# diagnostics
diag_lm <- fit_diagnostic_lm(base)
save_tab(white_test(diag_lm), "p2_white_test", "Heteroskedasticity test (linear model)")
diag_plots <- plot_residual_diagnostics(diag_lm)
save_fig(diag_plots$resid_fitted, "p2_resid_fitted")
save_fig(diag_plots$resid_qq, "p2_resid_qq")
