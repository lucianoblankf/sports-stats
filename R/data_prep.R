# Leitura e preparo dos dados do Understat

# Le o csv bruto e padroniza os nomes das colunas (xG -> xg, etc.)
read_understat <- function(path = "data/raw/understat.csv") {
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::rename(
      xg = x_g, npxg = npx_g, xa = x_a,
      xg_buildup = x_g_buildup, xg_chain = x_g_chain
    )
}

# Filtra atacantes por minimo de gols e/ou de xG e cria as variaveis derivadas
build_forwards <- function(data, goals_min = 0, xg_min = 0, main_position = "F") {
  data |>
    dplyr::filter(
      primary_position == main_position,
      goals >= goals_min,
      xg >= xg_min
    ) |>
    dplyr::mutate(
      goals_xg = goals / xg,
      npg_npxg = npg / npxg,
      goals_minus_xg = goals - xg,
      shots_90 = shots / time * 90,
      xg_90 = xg / time * 90,
      key_passes_90 = key_passes / time * 90,
      xa_90 = xa / time * 90,
      group_goals = dplyr::if_else(goals >= 20, "20 ou mais gols", "10 a 19 gols")
    )
}
