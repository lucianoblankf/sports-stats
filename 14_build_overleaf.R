# 14 - Build a single-file Overleaf version of the Part 2 report (tables inlined)

purrr::walk(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

inline_tables("report/part2.tex", "report/part2_overleaf.tex")
