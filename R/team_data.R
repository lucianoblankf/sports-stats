# Team-season data from Understat (used for the goalkeeper analysis)

understat_league_codes <- c(
  EPL = "EPL", La_Liga = "La_liga", Bundesliga = "Bundesliga",
  Serie_A = "Serie_A", Ligue_1 = "Ligue_1", RFPL = "RFPL"
)

# Downloads one league-season json (gzip) to data/raw/understat_teams/ if missing
download_team_json <- function(league, year, dir = "data/raw/understat_teams", pause = 1) {
  dest <- file.path(dir, paste0(league, "_", year, ".json.gz"))
  if (file.exists(dest)) {
    return(invisible(dest))
  }
  url <- paste0("https://understat.com/getLeagueData/",
                understat_league_codes[[league]], "/", year)
  utils::download.file(url, dest, mode = "wb", quiet = TRUE,
                       headers = c("X-Requested-With" = "XMLHttpRequest",
                                   "User-Agent" = "Mozilla/5.0"))
  Sys.sleep(pause)
  invisible(dest)
}

# Aggregates the match history of each team into one row per team-season
read_team_season <- function(path, league, year) {
  teams <- jsonlite::fromJSON(gzfile(path), simplifyVector = FALSE)$teams
  purrr::map(teams, \(tm) {
    hist <- tm$history
    tibble::tibble(
      league = league,
      year = year,
      team_id = tm$id,
      team_title = tm$title,
      matches = length(hist),
      xga = sum(purrr::map_dbl(hist, "xGA")),
      npxga = sum(purrr::map_dbl(hist, "npxGA")),
      ga = sum(purrr::map_dbl(hist, "missed")),
      xg_for = sum(purrr::map_dbl(hist, "xG")),
      goals_for = sum(purrr::map_dbl(hist, "scored"))
    )
  }) |>
    dplyr::bind_rows()
}
