# 01 - Download the aggregated Understat data (player x season) to data/raw

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()

url <- "https://raw.githubusercontent.com/vibedatascience/understat_players_aggregated/main/understat_players_aggregated_2014_td.csv"
raw_path <- "data/raw/understat.csv"

if (file.exists(raw_path)) {
  cli::cli_alert_info("{.file {raw_path}} already exists; skipping download (raw data is read-only).")
} else {
  cli::cli_alert_info("Downloading Understat data...")
  utils::download.file(url, raw_path, mode = "wb")
  cli::cli_alert_success("Data saved to {.file {raw_path}}")
}
