# 06 - Regressoes multivariadas com efeitos fixos (liga e temporada)
# Amostra principal: atacantes com xG >= 5 (corte no insumo, nao no resultado)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

understat <- readr::read_rds("data/processed/understat_clean.rds")
base_xg <- build_forwards(understat, xg_min = 5)
cli::cli_alert_info("Amostra xG >= 5: {nrow(base_xg)} observacoes")

ratio_models <- fit_ratio_models(base_xg)
save_texreg(ratio_models, "reg_gols_xg_xgmin5",
            "Determinantes da razao gols/xG (atacantes com xG $\\geq$ 5)")

calib_models <- fit_calibration_models(base_xg)
save_texreg(calib_models, "reg_calibracao_xgmin5",
            "Calibracao do xG: gols em funcao do xG (atacantes com xG $\\geq$ 5)")

# mesma especificacao na base com corte por gols (g10), para comparar o vies
base_g10 <- readr::read_rds("data/processed/forwards_g10.rds")
save_texreg(fit_calibration_models(base_g10), "reg_calibracao_g10",
            "Calibracao do xG na base com corte de 10 gols")

cli::cli_h2("Calibracao (xG >= 5)")
cli::cli_verbatim(texreg::screenreg(calib_models))
