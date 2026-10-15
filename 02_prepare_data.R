# 02 - Limpa os dados e cria as bases de atacantes (gols minimos 10 e 20)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()

understat <- read_understat()
leagues <- paste(sort(unique(understat$league)), collapse = ", ")
cli::cli_bullets(c(
  "i" = "{nrow(understat)} linhas, {ncol(understat)} colunas",
  "i" = "Ligas: {leagues}"
))

forwards_g10 <- build_forwards(understat, goals_min = 10)
forwards_g20 <- build_forwards(understat, goals_min = 20)

cli::cli_bullets(c(
  "v" = "Atacantes com >= 10 gols: {nrow(forwards_g10)} observacoes",
  "v" = "Atacantes com >= 20 gols: {nrow(forwards_g20)} observacoes"
))

readr::write_rds(understat, "data/processed/understat_clean.rds")
readr::write_rds(forwards_g10, "data/processed/forwards_g10.rds")
readr::write_rds(forwards_g20, "data/processed/forwards_g20.rds")

# copias em xlsx para o Power BI (pbi_gols_xg.pbix)
writexl::write_xlsx(forwards_g10, "data/processed/atacantes_10.xlsx")
writexl::write_xlsx(forwards_g20, "data/processed/atacantes_20.xlsx")
cli::cli_alert_success("Bases salvas em {.file data/processed/}")
