# Setup helpers: packages, folders and loading of project functions

ensure_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!purrr::map_lgl(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    cli::cli_alert_info("Instalando pacotes: {.pkg {missing_pkgs}}")
    pak::pak(missing_pkgs)
  }
  invisible(pkgs)
}

ensure_dirs <- function(dirs = c("data/raw", "data/processed", "tabs", "figs")) {
  purrr::walk(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
  invisible(dirs)
}

# Tema padrao dos graficos
set_project_theme <- function() {
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))
}
