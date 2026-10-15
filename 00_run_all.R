# Runs the whole pipeline in order. Run inside the project (Metodos_Estatisticos.Rproj).

scripts <- c(
  "01_download_data.R",
  "02_prepare_data.R",
  "03_descriptive.R",
  "04_tests_distribution.R",
  "05_selection_bias.R",
  "06_regression.R",
  "07_clusters.R",
  "08_download_team_data.R",
  "09_player_seasons.R",
  "10_regression_extended.R",
  "11_pca_index.R",
  "12_persistence.R",
  "13_goalkeepers.R"
)

for (script in scripts) {
  cli::cli_h1(script)
  source(script)
}

cli::cli_alert_success("Pipeline finished. See tabs/ and figs/.")
