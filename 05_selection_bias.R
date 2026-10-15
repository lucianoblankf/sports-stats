# 05 - Vies de selecao: corte por gols minimos vs corte por xG minimo
# Proximo passo do roteiro: testar cortes alternativos (xG minimo em vez de gols minimos)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

understat <- readr::read_rds("data/processed/understat_clean.rds")

# 1. Comparacao empirica de cortes
cuts <- compare_cuts(understat, goals_cuts = c(5, 8, 10, 12, 15, 20),
                     xg_cuts = c(3, 5, 7, 10, 12, 15))
save_tab(cuts, "cortes_comparacao",
         "Gols/xG medio sob cortes por gols minimos e por xG minimo")
save_fig(plot_cut_comparison(cuts), "cortes_comparacao", width = 8, height = 4.5)

# 2. Simulacao: sem habilidade (gols ~ Poisson(xG)), o corte por gols infla a razao?
sim <- simulate_selection(understat, goals_cuts = c(5, 8, 10, 12, 15, 20), n_sim = 500)
save_tab(sim, "simulacao_vies",
         "Razao media gols/xG simulada sem habilidade (gols ~ Poisson(xG))")

real_g10 <- round(cuts$mean[cuts$rule == "gols_min" & cuts$threshold == 10], 3)
sim_g10 <- round(sim$mean_ratio[sim$goals_min == 10], 3)
cli::cli_bullets(c(
  "i" = "Corte de 10 gols (dados reais): media gols/xG = {real_g10}",
  "i" = "Corte de 10 gols (simulado, sem habilidade): media gols/xG = {sim_g10}"
))
