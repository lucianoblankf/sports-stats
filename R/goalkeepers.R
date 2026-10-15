# Part 2: goalkeeper analysis (goals conceded vs xGA at the team level)
# The player data has no PSxG or saves, so the goalkeeper effect is proxied by the
# team's goals conceded relative to xGA, attributed to the main goalkeeper.

# One row per team-season with its main goalkeeper (>= min_share of the team's minutes)
build_gk_seasons <- function(understat, team_seasons, min_share = 0.6) {
  gk <- understat |>
    dplyr::filter(primary_position == "GK", year <= 2024) |>
    dplyr::select(id, player_name, team_title, league, year, season, gk_games = games, gk_time = time)

  joined <- dplyr::inner_join(gk, team_seasons, by = c("league", "year", "team_title"))
  n_unmatched <- nrow(gk) - nrow(joined)
  if (n_unmatched > 0) {
    cli::cli_alert_warning("{n_unmatched} goalkeeper rows without a matching team-season")
  }

  joined |>
    dplyr::mutate(share = gk_time / (matches * 90)) |>
    dplyr::filter(share >= min_share) |>
    dplyr::slice_max(gk_time, n = 1, by = c(league, year, team_id), with_ties = FALSE) |>
    dplyr::mutate(
      ga_xga = ga / xga,
      goals_prevented = xga - ga,
      goals_prevented_90 = goals_prevented / matches,
      xga_90 = xga / matches,
      ga_90 = ga / matches
    )
}

# Leave-one-season-out skill: the goalkeeper's ga/xga ratio in all OTHER seasons.
# "Elite" = lowest quartile of that prior ratio (needs >= 2 other seasons), so the
# group is defined out of sample and the tested season is not used to select it.
add_prior_skill <- function(gk, min_prior_seasons = 2, elite_quantile = 0.25) {
  gk <- gk |>
    dplyr::mutate(
      tot_ga = sum(ga), tot_xga = sum(xga), n_seasons = dplyr::n(),
      .by = id
    ) |>
    dplyr::mutate(
      prior_n = n_seasons - 1,
      prior_ratio = dplyr::if_else(prior_n >= min_prior_seasons,
                                   (tot_ga - ga) / (tot_xga - xga), NA_real_)
    )
  cutoff <- stats::quantile(gk$prior_ratio, elite_quantile, na.rm = TRUE)
  gk |>
    dplyr::mutate(
      elite = dplyr::case_when(
        is.na(prior_ratio) ~ NA,
        prior_ratio <= cutoff ~ TRUE,
        TRUE ~ FALSE
      ),
      elite_label = dplyr::case_when(
        is.na(elite) ~ NA_character_,
        elite ~ "Elite (prior ratio, bottom quartile)",
        TRUE ~ "Other"
      )
    )
}

gk_group_summary <- function(gk) {
  gk |>
    dplyr::filter(!is.na(elite)) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_ga_xga = mean(ga_xga),
      median_ga_xga = stats::median(ga_xga),
      sd_ga_xga = stats::sd(ga_xga),
      mean_goals_prevented = mean(goals_prevented),
      mean_xga_90 = mean(xga_90),
      mean_prior_ratio = mean(prior_ratio),
      .by = elite_label
    )
}

run_gk_tests <- function(gk) {
  eligible <- dplyr::filter(gk, !is.na(elite))
  elite <- dplyr::filter(eligible, elite)
  other <- dplyr::filter(eligible, !elite)
  dplyr::bind_rows(
    htest_row("Elite goalkeepers: mean ga/xGA equals 1", "Mean below 1",
              stats::t.test(elite$ga_xga, mu = 1, alternative = "less")),
    htest_row("Elite goalkeepers: median ga/xGA equals 1", "Median below 1",
              stats::wilcox.test(elite$ga_xga, mu = 1, alternative = "less")),
    htest_row("Other goalkeepers: mean ga/xGA equals 1", "Mean differs from 1",
              stats::t.test(other$ga_xga, mu = 1)),
    htest_row("Mean ga/xGA is equal for elite and other goalkeepers", "Elite mean is lower",
              stats::t.test(elite$ga_xga, other$ga_xga, alternative = "less")),
    htest_row("Distribution of ga/xGA is equal for elite and other goalkeepers",
              "Elite distribution is lower",
              stats::wilcox.test(elite$ga_xga, other$ga_xga, alternative = "less")),
    htest_row("Mean xGA per match is equal for elite and other goalkeepers",
              "Means differ",
              stats::t.test(elite$xga_90, other$xga_90))
  )
}

fit_gk_models <- function(gk) {
  eligible <- dplyr::filter(gk, !is.na(elite)) |>
    dplyr::mutate(elite_num = as.integer(elite))
  # Note: goalkeeper/team fixed effects are deliberately NOT used with the
  # leave-one-season-out measure, because within a goalkeeper it is mechanically
  # negatively related to the season outcome.
  list(
    "(1) goals conceded" = fixest::feols(
      ga ~ xga + elite_num | league + season, data = eligible, cluster = ~team_id),
    "(2) ga/xGA, elite" = fixest::feols(
      ga_xga ~ elite_num | league + season, data = eligible, cluster = ~team_id),
    "(3) ga/xGA, prior ratio" = fixest::feols(
      ga_xga ~ prior_ratio | league + season, data = eligible, cluster = ~team_id),
    "(4) xGA per match, elite" = fixest::feols(
      xga_90 ~ elite_num | league + season, data = eligible, cluster = ~team_id)
  )
}

plot_gk_box <- function(gk) {
  gk |>
    dplyr::filter(!is.na(elite)) |>
    ggplot2::ggplot(ggplot2::aes(elite_label, ga_xga)) +
    ggplot2::geom_boxplot(fill = "grey90", outlier.alpha = 0.4) +
    ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    ggplot2::labs(x = NULL, y = "Goals conceded / xGA (team season)")
}
