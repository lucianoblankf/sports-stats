# 15 - Builds the Power BI project (powerbi/sports_stats.pbip) from the Part 1 and Part 2 outputs.
# Open the .pbip in Power BI Desktop and click Refresh. If the data folder moves,
# change the DataFolder parameter (Transform data > Edit parameters).

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
ensure_dirs(c("data/processed/powerbi", "powerbi"))

# ---- 1. datasets -----------------------------------------------------------------------

indexed <- readr::read_rds("data/processed/p2_forwards_indexed.rds")
gk <- readr::read_rds("data/processed/p2_goalkeepers.rds")
cuts <- readr::read_csv("tabs/cut_comparison.csv", show_col_types = FALSE)
sim <- readr::read_csv("tabs/bias_simulation.csv", show_col_types = FALSE)

datasets <- list(
  player_seasons = pbi_player_seasons(indexed),
  persistence_pairs = pbi_persistence_pairs(indexed),
  persistence_summary = readr::read_csv("tabs/p2_persistence.csv", show_col_types = FALSE),
  bias_goals_cut = pbi_bias_goals_cut(cuts, sim) |> dplyr::mutate(threshold = as.integer(threshold), n = as.integer(n)),
  bias_xg_cut = pbi_bias_xg_cut(cuts) |> dplyr::mutate(threshold = as.integer(threshold), n = as.integer(n)),
  pca_variance = readr::read_csv("tabs/p2_pca_variance.csv", show_col_types = FALSE),
  pca_loadings = readr::read_csv("tabs/p2_pca_loadings.csv", show_col_types = FALSE),
  career_index = pbi_career_index(indexed),
  gk_seasons = pbi_gk_seasons(gk),
  gk_career = pbi_gk_career(gk)
)

# integer-valued id/count columns are exported as integers
datasets <- purrr::map(datasets, \(d) {
  d |> dplyr::mutate(dplyr::across(
    dplyr::any_of(c("row_id", "id", "year", "goals", "shots", "time", "seasons", "n_pairs",
                    "ga", "n")),
    \(x) as.integer(round(x))
  ))
})

purrr::iwalk(datasets, \(d, nm) {
  readr::write_csv(d, file.path("data/processed/powerbi", paste0(nm, ".csv")), na = "")
})
cli::cli_alert_success("{length(datasets)} datasets exported to {.file data/processed/powerbi/}")

# ---- 2. semantic model ------------------------------------------------------------------

m <- function(name, expr, fmt = NULL) list(name = name, expr = expr, fmt = fmt)

measures <- list(
  player_seasons = list(
    m("Player seasons", "COUNTROWS(player_seasons)", "#,0"),
    m("Goals/xG (total)", "DIVIDE(SUM(player_seasons[goals]), SUM(player_seasons[xg]))", "0.000"),
    m("Mean goals/xG", "AVERAGE(player_seasons[goals_xg])", "0.000"),
    m("Mean xG per 90", "AVERAGE(player_seasons[xg_90])", "0.000")
  ),
  bias_goals_cut = list(
    m("Real mean (10 goals)", "CALCULATE(MAX(bias_goals_cut[real_mean]), bias_goals_cut[threshold] = 10)", "0.000"),
    m("Simulated mean (10 goals)", "CALCULATE(MAX(bias_goals_cut[simulated_mean]), bias_goals_cut[threshold] = 10)", "0.000")
  ),
  bias_xg_cut = list(
    m("Real mean (xG >= 5)", "CALCULATE(MAX(bias_xg_cut[real_mean]), bias_xg_cut[threshold] = 5)", "0.000")
  ),
  persistence_summary = list(
    m("r goals/xG", "CALCULATE(MAX(persistence_summary[pearson_r]), persistence_summary[variable] = \"goals_xg\")", "0.00"),
    m("r xG per 90", "CALCULATE(MAX(persistence_summary[pearson_r]), persistence_summary[variable] = \"xg_90\")", "0.00")
  ),
  gk_seasons = list(
    m("Team-seasons", "COUNTROWS(gk_seasons)", "#,0"),
    m("Mean ga/xGA", "AVERAGE(gk_seasons[ga_xga])", "0.000"),
    m("Elite mean ga/xGA", "CALCULATE(AVERAGE(gk_seasons[ga_xga]), gk_seasons[elite_group] = \"Elite (out of sample)\")", "0.000"),
    m("Other mean ga/xGA", "CALCULATE(AVERAGE(gk_seasons[ga_xga]), gk_seasons[elite_group] = \"Other\")", "0.000")
  )
)

no_sum <- c("row_id", "id", "year", "threshold", "seasons", "n", "n_pairs")
tables <- purrr::imap(datasets, \(d, nm) {
  tmdl_table(d, nm, measures = measures[[nm]] %||% list(), no_sum = no_sum)
})

unlink("powerbi/sports_stats.SemanticModel", recursive = TRUE)
write_semantic_model("powerbi/sports_stats.SemanticModel", tables,
                     data_folder = paste0(normalizePath("data/processed/powerbi"), "\\"))

# ---- 3. report pages -----------------------------------------------------------------------

page_title <- function(title, subtitle) {
  list(
    v_text("title", 20, 8, 1240, 44, title, size = 22, bold = TRUE, color = "#1F3A4B"),
    v_text("subtitle", 20, 50, 1240, 30, subtitle, size = 11, color = "#5A6B78")
  )
}

pages <- list(
  page_overview = list(
    title = "Overview",
    visuals = c(
      page_title("Goals vs xG: forwards in Europe's top leagues (2014/15-2024/25)",
                 "Forwards with season xG >= 5, transfers merged by player and season. Source: Understat."),
      list(
        v_slicer("slicer_league", 20, 90, 260, 110, "player_seasons", "league", "League"),
        v_slicer("slicer_season", 300, 90, 260, 110, "player_seasons", "season", "Season"),
        v_card("card_n", 600, 90, 200, 110, "player_seasons", "Player seasons", "Player-seasons"),
        v_card("card_ratio", 820, 90, 200, 110, "player_seasons", "Goals/xG (total)", "Goals / xG (total)"),
        v_card("card_xg90", 1040, 90, 220, 110, "player_seasons", "Mean xG per 90", "Mean xG per 90"),
        v_scatter("scatter_xg_goals", 20, 215, 620, 485, proj_col("player_seasons", "row_id"),
                  proj_sum("player_seasons", "xg", "xG"), proj_sum("player_seasons", "goals", "Goals"),
                  "xG vs goals (each point = one player-season)",
                  series = proj_col("player_seasons", "league")),
        v_column("column_league", 660, 215, 600, 230, proj_col("player_seasons", "league"),
                 list(proj_meas("player_seasons", "Mean goals/xG", "Mean goals/xG")),
                 "Mean goals/xG by league (axis starts at 0.9)", axis_start = 0.9, axis_end = 1.1),
        v_line("line_season", 660, 460, 600, 240, proj_col("player_seasons", "season"),
               list(proj_meas("player_seasons", "Mean goals/xG", "Mean goals/xG")), "Mean goals/xG by season")
      )
    )
  ),
  page_selection = list(
    title = "Selection bias",
    visuals = c(
      page_title("Why filtering on goals inflates goals/xG",
                 "Real data vs a simulation where every forward finishes exactly as xG predicts (no skill)."),
      list(
        v_card("card_real10", 20, 90, 300, 110, "bias_goals_cut", "Real mean (10 goals)", "Real mean, cutoff 10 goals"),
        v_card("card_sim10", 340, 90, 300, 110, "bias_goals_cut", "Simulated mean (10 goals)", "Simulated mean, no skill, 10 goals"),
        v_card("card_xg5", 660, 90, 300, 110, "bias_xg_cut", "Real mean (xG >= 5)", "Real mean, cutoff xG >= 5"),
        v_line("line_goals_cut", 20, 215, 620, 485, proj_col("bias_goals_cut", "threshold"),
               list(proj_sum("bias_goals_cut", "real_mean", "Real data"),
                    proj_sum("bias_goals_cut", "simulated_mean", "Simulated (no skill)")),
               "Minimum-goals cutoff: mean goals/xG"),
        v_line("line_xg_cut", 660, 215, 600, 485, proj_col("bias_xg_cut", "threshold"),
               list(proj_sum("bias_xg_cut", "real_mean", "Real data")),
               "Minimum-xG cutoff: mean goals/xG")
      )
    )
  ),
  page_persistence = list(
    title = "Skill vs luck",
    visuals = c(
      page_title("Is finishing a stable skill? Season-to-season persistence",
                 "Correlation of each metric between season t and t+1 for the same forward (1,407 pairs)."),
      list(
        v_card("card_r_gxg", 20, 90, 300, 110, "persistence_summary", "r goals/xG", "Correlation of goals/xG"),
        v_card("card_r_xg90", 340, 90, 300, 110, "persistence_summary", "r xG per 90", "Correlation of xG per 90"),
        v_column("bar_persistence", 20, 215, 620, 485, proj_col("persistence_summary", "variable"),
                 list(proj_sum("persistence_summary", "pearson_r", "Correlation (t, t+1)")),
                 "Persistence by metric", type = "clusteredBarChart",
                 sort = list(field = f_agg("persistence_summary", "pearson_r", 0), direction = "Descending")),
        v_scatter("scatter_p_gxg", 660, 90, 600, 300, proj_col("persistence_pairs", "row_id"),
                  proj_sum("persistence_pairs", "goals_xg_t", "Goals/xG, season t"),
                  proj_sum("persistence_pairs", "goals_xg_t1", "Goals/xG, season t+1"),
                  "Goals/xG persists weakly"),
        v_scatter("scatter_p_xg90", 660, 400, 600, 300, proj_col("persistence_pairs", "row_id"),
                  proj_sum("persistence_pairs", "xg_90_t", "xG per 90, season t"),
                  proj_sum("persistence_pairs", "xg_90_t1", "xG per 90, season t+1"),
                  "xG per 90 persists strongly")
      )
    )
  ),
  page_pca = list(
    title = "PCA and index",
    visuals = c(
      page_title("PCA of finishing and creation metrics, and composite offensive index",
                 "Seven standardized metrics; index = mean of finishing, chance volume and creation z-scores."),
      list(
        v_column("column_variance", 20, 90, 420, 300, proj_col("pca_variance", "component"),
                 list(proj_sum("pca_variance", "variance_pct", "Variance explained (%)")),
                 "Variance explained by component"),
        v_table("table_loadings", 460, 90, 800, 300,
                list(proj_col("pca_loadings", "variable", label = "Metric"), proj_sum("pca_loadings", "PC1", "PC1"),
                     proj_sum("pca_loadings", "PC2", "PC2"), proj_sum("pca_loadings", "PC3", "PC3"),
                     proj_sum("pca_loadings", "PC4", "PC4")),
                "PCA loadings"),
        v_table("table_career_index", 20, 410, 1240, 290,
                list(proj_col("career_index", "player_name", label = "Player"), proj_sum("career_index", "seasons", "Seasons"),
                     proj_sum("career_index", "goals", "Goals"), proj_sum("career_index", "xg", "xG"),
                     proj_sum("career_index", "goals_xg", "Goals/xG"),
                     proj_sum("career_index", "index_equal", "Equal-weight index"),
                     proj_sum("career_index", "index_pca", "PCA index")),
                "Forwards ranked by mean composite index (3+ seasons)",
                sort = list(field = f_agg("career_index", "index_equal", 0), direction = "Descending"))
      )
    )
  ),
  page_goalkeepers = list(
    title = "Goalkeepers",
    visuals = c(
      page_title("Do elite goalkeepers concede fewer goals than xGA?",
                 "Team-level proxy: goals conceded / xGA. Elite = lowest quartile of the goalkeeper's ratio in other seasons."),
      list(
        v_card("card_gk_n", 20, 90, 300, 110, "gk_seasons", "Team-seasons", "Team-seasons"),
        v_card("card_gk_elite", 340, 90, 300, 110, "gk_seasons", "Elite mean ga/xGA", "Elite goalkeepers, ga/xGA"),
        v_card("card_gk_other", 660, 90, 300, 110, "gk_seasons", "Other mean ga/xGA", "Other goalkeepers, ga/xGA"),
        v_column("column_gk", 20, 215, 400, 485, proj_col("gk_seasons", "elite_group"),
                 list(proj_meas("gk_seasons", "Mean ga/xGA", "Mean ga/xGA")),
                 "Mean goals conceded / xGA (axis starts at 0.8)", axis_start = 0.8, axis_end = 1.1),
        v_scatter("scatter_gk", 440, 215, 440, 485, proj_col("gk_seasons", "row_id"),
                  proj_sum("gk_seasons", "xga", "xGA"), proj_sum("gk_seasons", "ga", "Goals conceded"),
                  "Goals conceded vs xGA (team-season)",
                  series = proj_col("gk_seasons", "elite_group")),
        v_table("table_gk_career", 900, 215, 360, 485,
                list(proj_col("gk_career", "player_name", label = "Goalkeeper"), proj_sum("gk_career", "seasons", "Seasons"),
                     proj_sum("gk_career", "ga_xga", "ga/xGA")),
                "Career ga/xGA (4+ seasons)",
                sort = list(field = f_agg("gk_career", "ga_xga", 0), direction = "Ascending"))
      )
    )
  )
)

unlink("powerbi/sports_stats.Report", recursive = TRUE)
write_report("powerbi/sports_stats.Report", pages, template_dir = "powerbi/template")

jsonlite::write_json(
  list(version = "1.0", artifacts = list(list(report = list(path = "sports_stats.Report"))),
       settings = list(enableAutoRecovery = TRUE)),
  "powerbi/sports_stats.pbip", auto_unbox = TRUE, pretty = TRUE
)
cli::cli_alert_success("Power BI project written: {.file powerbi/sports_stats.pbip}")
