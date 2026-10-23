# Power BI semantic model (TMDL) generators

tmdl_m_type <- function(x) {
  if (is.integer(x)) {
    "Int64.Type"
  } else if (is.numeric(x)) {
    "type number"
  } else if (is.logical(x)) {
    "type logical"
  } else {
    "type text"
  }
}

tmdl_data_type <- function(x) {
  if (is.integer(x)) {
    "int64"
  } else if (is.numeric(x)) {
    "double"
  } else if (is.logical(x)) {
    "boolean"
  } else {
    "string"
  }
}

# Quotes a TMDL name when it has spaces or special characters
tmdl_name <- function(x) {
  dplyr::if_else(stringr::str_detect(x, "^[A-Za-z_][A-Za-z0-9_]*$"), x, paste0("'", x, "'"))
}

# Builds the .tmdl text of one table: columns, measures and a csv-backed partition.
# `measures` is a list of list(name =, expr =, fmt =).
tmdl_table <- function(data, name, measures = list(), no_sum = character()) {
  col_blocks <- purrr::map_chr(names(data), \(col) {
    x <- data[[col]]
    summarize <- if (is.numeric(x) && !col %in% no_sum) "sum" else "none"
    fmt <- if (is.integer(x)) "\n\t\tformatString: 0" else ""
    paste0("\tcolumn ", tmdl_name(col), "\n\t\tdataType: ", tmdl_data_type(x), fmt,
           "\n\t\tsummarizeBy: ", summarize, "\n\t\tsourceColumn: ", col, "\n")
  })

  measure_blocks <- purrr::map_chr(measures, \(m) {
    paste0("\tmeasure ", tmdl_name(m$name), " = ", m$expr,
           if (!is.null(m$fmt)) paste0("\n\t\tformatString: ", m$fmt) else "", "\n")
  })

  types <- purrr::map_chr(names(data), \(col) {
    paste0("{\"", col, "\", ", tmdl_m_type(data[[col]]), "}")
  }) |>
    paste(collapse = ", ")

  partition <- paste0(
    "\tpartition ", tmdl_name(name), " = m\n",
    "\t\tmode: import\n",
    "\t\tsource =\n",
    "\t\t\t\tlet\n",
    "\t\t\t\t    Source = Csv.Document(File.Contents(DataFolder & \"", name, ".csv\"), [Delimiter=\",\", Encoding=65001, QuoteStyle=QuoteStyle.Csv]),\n",
    "\t\t\t\t    Headers = Table.PromoteHeaders(Source, [PromoteAllScalars=true]),\n",
    "\t\t\t\t    Typed = Table.TransformColumnTypes(Headers, {", types, "}, \"en-US\")\n",
    "\t\t\t\tin\n",
    "\t\t\t\t    Typed\n"
  )

  paste0("table ", tmdl_name(name), "\n\n",
         paste(c(measure_blocks, col_blocks), collapse = "\n"), "\n", partition)
}

# Writes the whole semantic model folder
write_semantic_model <- function(model_dir, tables, data_folder) {
  def_dir <- file.path(model_dir, "definition")
  dir.create(file.path(def_dir, "tables"), recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(list(version = "4.2", settings = structure(list(), names = character())),
                       file.path(model_dir, "definition.pbism"), auto_unbox = TRUE)

  writeLines("database\n\tcompatibilityLevel: 1600\n", file.path(def_dir, "database.tmdl"))

  table_names <- names(tables)
  model_lines <- c(
    "model Model", "\tculture: en-US", "\tdefaultPowerBIDataSourceVersion: powerBI_V3",
    "\tsourceQueryCulture: en-US", "",
    paste0("ref table ", tmdl_name(table_names)),
    "ref expression DataFolder", ""
  )
  writeLines(model_lines, file.path(def_dir, "model.tmdl"))

  writeLines(
    paste0("expression DataFolder = \"", data_folder,
           "\" meta [IsParameterQuery=true, Type=\"Text\", IsParameterQueryRequired=true]\n"),
    file.path(def_dir, "expressions.tmdl")
  )

  purrr::iwalk(tables, \(tbl, nm) {
    writeLines(tbl, file.path(def_dir, "tables", paste0(nm, ".tmdl")), useBytes = TRUE)
  })
  invisible(model_dir)
}
