# Descriptive statistics

describe_var <- function(x) {
  q <- as.numeric(stats::quantile(x, c(0.25, 0.75)))
  iqr <- q[2] - q[1]
  tibble::tibble(
    n = length(x),
    mean = mean(x),
    median = stats::median(x),
    sd = stats::sd(x),
    cv = stats::sd(x) / mean(x),
    min = min(x),
    q1 = q[1],
    q3 = q[2],
    max = max(x),
    iqr = iqr,
    skewness = moments::skewness(x),
    excess_kurtosis = moments::kurtosis(x) - 3,
    outliers = sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
  )
}

describe_vars <- function(data, vars) {
  purrr::map(vars, \(v) dplyr::bind_cols(variable = v, describe_var(data[[v]]))) |>
    dplyr::bind_rows()
}

# Highest and lowest goals/xG ratios
extreme_players <- function(data, n = 10) {
  dplyr::bind_rows(
    data |> dplyr::arrange(dplyr::desc(goals_xg)) |>
      dplyr::slice_head(n = n) |> dplyr::mutate(type = "Highest"),
    data |> dplyr::arrange(goals_xg) |>
      dplyr::slice_head(n = n) |> dplyr::mutate(type = "Lowest")
  ) |>
    dplyr::select(type, player_name, season, team_title, league, goals, xg, goals_xg)
}

summarise_by_league <- function(data) {
  data |>
    dplyr::summarise(
      n = dplyr::n(), mean = mean(goals_xg), median = stats::median(goals_xg),
      sd = stats::sd(goals_xg), .by = league
    )
}
