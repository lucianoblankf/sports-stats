# Helpers to save tables and figures

# Saves a table to tabs/ as .csv, .rds and .tex (kable/booktabs)
save_tab <- function(tab, name, caption, tab_tex = tab) {
  readr::write_csv(tab, file.path("tabs", paste0(name, ".csv")))
  readr::write_rds(tab, file.path("tabs", paste0(name, ".rds")))
  tab_tex |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3))) |>
    knitr::kable(format = "latex", booktabs = TRUE, caption = caption,
                 label = name) |>
    as.character() |>
    writeLines(file.path("tabs", paste0(name, ".tex")))
  cli::cli_alert_success("Table saved: {.file tabs/{name}}")
  invisible(tab)
}

# Saves a figure to figs/ as .png (300 dpi)
save_fig <- function(plot, name, width = 7, height = 4.5) {
  ggplot2::ggsave(file.path("figs", paste0(name, ".png")), plot,
                  width = width, height = height, dpi = 300)
  cli::cli_alert_success("Figure saved: {.file figs/{name}.png}")
  invisible(plot)
}
