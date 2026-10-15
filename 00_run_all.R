# Executa todo o pipeline na ordem. Rodar dentro do projeto (Metodos_Estatisticos.Rproj).

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

cli::cli_alert_success("Pipeline concluido. Veja tabs/ e figs/.")
