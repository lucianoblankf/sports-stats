# Reading and preparing the Understat data

# Reads the raw csv and standardises column names (xG -> xg, etc.)
read_understat <- function(path = "data/raw/understat.csv") {
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::rename(
      xg = x_g, npxg = npx_g, xa = x_a,
      xg_buildup = x_g_buildup, xg_chain = x_g_chain
    )
}

# Filters forwards by minimum goals and/or minimum xG and builds derived variables
# Incomplete seasons (default: 2025/26, still in progress) are dropped
build_forwards <- function(data, goals_min = 0, xg_min = 0, main_position = "F",
                           exclude_seasons = "2025/26") {
  data |>
    dplyr::filter(
      !season %in% exclude_seasons,
      primary_position == main_position,
      goals >= goals_min,
      xg >= xg_min
    ) |>
    add_derived_vars()
}

add_derived_vars <- function(data) {
  data |>
    dplyr::mutate(
      goals_xg = goals / xg,
      npg_npxg = npg / npxg,
      goals_minus_xg = goals - xg,
      shots_90 = shots / time * 90,
      xg_90 = xg / time * 90,
      key_passes_90 = key_passes / time * 90,
      xa_90 = xa / time * 90,
      goals_90 = goals / time * 90,
      xg_per_shot = xg / shots,
      xg_buildup_90 = xg_buildup / time * 90,
      xg_chain_90 = xg_chain / time * 90,
      group_goals = dplyr::if_else(goals >= 20, "20+ goals", "10-19 goals")
    )
}

# Part 2: one row per player-season (clubs merged for players who transferred).
# League is the one where the player had more minutes; primary_position is
# kept only when the player was classified the same way at every club.
merge_player_seasons <- function(data, exclude_seasons = "2025/26") {
  data |>
    dplyr::filter(!season %in% exclude_seasons) |>
    dplyr::arrange(id, season, dplyr::desc(time)) |>
    dplyr::summarise(
      player_name = dplyr::first(player_name),
      team_title = paste(unique(team_title), collapse = " / "),
      league = dplyr::first(league),
      year = dplyr::first(year),
      n_clubs = dplyr::n(),
      position = paste(sort(unique(unlist(stringr::str_split(position, " ")))), collapse = " "),
      primary_position = if (dplyr::n_distinct(primary_position) == 1) {
        dplyr::first(primary_position)
      } else {
        "mixed"
      },
      dplyr::across(c(games, time, goals, npg, shots, key_passes, xg, npxg, assists,
                      xa, xg_buildup, xg_chain), sum),
      .by = c(id, season)
    )
}

build_forwards_merged <- function(data, goals_min = 0, xg_min = 0) {
  merge_player_seasons(data) |>
    dplyr::filter(primary_position == "F", goals >= goals_min, xg >= xg_min) |>
    add_derived_vars()
}
