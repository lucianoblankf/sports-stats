# 06 - Multivariate regressions with fixed effects (league and season)
# Main sample: forwards with xG >= 5 (cutoff on the input, not on the outcome)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

understat <- readr::read_rds("data/processed/understat_clean.rds")
base_xg <- build_forwards(understat, xg_min = 5)
cli::cli_alert_info("Sample xG >= 5: {nrow(base_xg)} observations")

ratio_models <- fit_ratio_models(base_xg)
save_texreg(ratio_models, "reg_ratio_xgmin5",
            "Determinants of the goals/xG ratio (forwards with xG $\\geq$ 5)")

calib_models <- fit_calibration_models(base_xg)
save_texreg(calib_models, "reg_calibration_xgmin5",
            "xG calibration: goals as a function of xG (forwards with xG $\\geq$ 5)")

# same specification on the goals-cutoff dataset (g10), to compare the bias
base_g10 <- readr::read_rds("data/processed/forwards_g10.rds")
save_texreg(fit_calibration_models(base_g10), "reg_calibration_g10",
            "xG calibration on the 10-goal cutoff dataset")

cli::cli_h2("Calibration (xG >= 5)")
cli::cli_verbatim(texreg::screenreg(calib_models))
