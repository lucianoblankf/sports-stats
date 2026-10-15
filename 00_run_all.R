# Runs the whole pipeline in order. Run inside the project (Metodos_Estatisticos.Rproj).

scripts <- c(
  "01_download_data.R",
  "02_prepare_data.R",
  "03_descriptive.R",
  "04_tests_distribution.R",
  "05_selection_bias.R",
  "06_regression.R",
  "07_clusters.R"
)

for (script in scripts) {
  cli::cli_h1(script)
  source(script)
}

cli::cli_alert_success("Pipeline finished. See tabs/ and figs/.")
