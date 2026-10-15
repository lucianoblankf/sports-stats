# 02 - Clean the data and build the forward datasets (minimum 10 and 20 goals)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs()

understat <- read_understat()
leagues <- paste(sort(unique(understat$league)), collapse = ", ")
cli::cli_bullets(c(
  "i" = "{nrow(understat)} rows, {ncol(understat)} columns",
  "i" = "Leagues: {leagues}"
))

forwards_g10 <- build_forwards(understat, goals_min = 10)
forwards_g20 <- build_forwards(understat, goals_min = 20)

cli::cli_bullets(c(
  "v" = "Forwards with >= 10 goals: {nrow(forwards_g10)} observations",
  "v" = "Forwards with >= 20 goals: {nrow(forwards_g20)} observations"
))

readr::write_rds(understat, "data/processed/understat_clean.rds")
readr::write_rds(forwards_g10, "data/processed/forwards_g10.rds")
readr::write_rds(forwards_g20, "data/processed/forwards_g20.rds")

# xlsx copies for Power BI (pbi_gols_xg.pbix)
writexl::write_xlsx(forwards_g10, "data/processed/forwards_10.xlsx")
writexl::write_xlsx(forwards_g20, "data/processed/forwards_20.xlsx")
cli::cli_alert_success("Datasets saved to {.file data/processed/}")
