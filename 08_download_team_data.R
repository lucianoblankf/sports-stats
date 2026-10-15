# 08 - Part 2: download team-season data (goals conceded and xGA) from Understat
# Needed for the goalkeeper analysis (player data has no goals conceded or PSxG)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs(c("data/raw/understat_teams", "data/processed"))

grid <- tidyr::expand_grid(league = names(understat_league_codes), year = 2014:2024)
cli::cli_alert_info("Fetching {nrow(grid)} league-seasons (existing files are skipped)")

purrr::walk2(grid$league, grid$year, download_team_json)

team_seasons <- purrr::map2(grid$league, grid$year, \(l, y) {
  read_team_season(file.path("data/raw/understat_teams", paste0(l, "_", y, ".json.gz")), l, y)
}) |>
  dplyr::bind_rows()

cli::cli_alert_success("{nrow(team_seasons)} team-seasons")
readr::write_rds(team_seasons, "data/processed/team_seasons.rds")
