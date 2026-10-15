# 05 - Selection bias: minimum-goals cutoff vs minimum-xG cutoff
# Next step from the course outline: test alternative cutoffs (minimum xG instead of minimum goals)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()
set_project_theme()
set.seed(123)

understat <- readr::read_rds("data/processed/understat_clean.rds")

# 1. Empirical comparison of cutoffs
cuts <- compare_cuts(understat, goals_cuts = c(5, 8, 10, 12, 15, 20),
                     xg_cuts = c(3, 5, 7, 10, 12, 15))
save_tab(cuts, "cut_comparison",
         "Mean goals/xG under minimum-goals and minimum-xG cutoffs")
save_fig(plot_cut_comparison(cuts), "cut_comparison", width = 8, height = 4.5)

# 2. Simulation: with no skill (goals ~ Poisson(xG)), does a goals cutoff inflate the ratio?
sim <- simulate_selection(understat, goals_cuts = c(5, 8, 10, 12, 15, 20), n_sim = 500)
save_tab(sim, "bias_simulation",
         "Simulated mean goals/xG with no skill (goals ~ Poisson(xG))")

real_g10 <- round(cuts$mean[cuts$rule == "min_goals" & cuts$threshold == 10], 3)
sim_g10 <- round(sim$mean_ratio[sim$goals_min == 10], 3)
cli::cli_bullets(c(
  "i" = "10-goal cutoff (real data): mean goals/xG = {real_g10}",
  "i" = "10-goal cutoff (simulated, no skill): mean goals/xG = {sim_g10}"
))
