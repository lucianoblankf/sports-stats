# LaTeX helpers

# Builds a single-file version of a .tex report: every \input{...} of a table file is
# replaced by that file's content, and the graphics path accepts flat or figs/ uploads.
# Useful for Overleaf when only the .tex file and the figures are uploaded.
inline_tables <- function(tex_in, tex_out, tabs_dir = "tabs") {
  txt <- paste(readLines(tex_in, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  inputs <- regmatches(txt, gregexpr("\\\\input\\{\\.\\./tabs/[^}]+\\}", txt))[[1]]
  for (inp in unique(inputs)) {
    file_name <- sub("\\\\input\\{\\.\\./tabs/([^}]+)\\}", "\\1", inp)
    content <- paste(readLines(file.path(tabs_dir, file_name), warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
    # split/paste keeps backslashes literal (gsub would treat them as escapes)
    txt <- paste(strsplit(txt, inp, fixed = TRUE)[[1]], collapse = content)
  }
  txt <- paste(strsplit(txt, "\\graphicspath{{../figs/}}", fixed = TRUE)[[1]],
               collapse = "\\graphicspath{{figs/}{./}{../figs/}}")
  writeLines(txt, tex_out, useBytes = TRUE)
  cli::cli_alert_success("Single-file report written: {.file {tex_out}} ({length(unique(inputs))} tables inlined)")
  invisible(tex_out)
}
