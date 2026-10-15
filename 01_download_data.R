# 01 - Baixa a base agregada do Understat (jogador x temporada) para data/raw

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()

url <- "https://raw.githubusercontent.com/vibedatascience/understat_players_aggregated/main/understat_players_aggregated_2014_td.csv"
raw_path <- "data/raw/understat.csv"

if (file.exists(raw_path)) {
  cli::cli_alert_info("{.file {raw_path}} ja existe; download ignorado (dados brutos sao somente leitura).")
} else {
  cli::cli_alert_info("Baixando dados do Understat...")
  utils::download.file(url, raw_path, mode = "wb")
  cli::cli_alert_success("Dados salvos em {.file {raw_path}}")
}
