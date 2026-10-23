# Power BI report (PBIR) builders: fields, projections, visuals, pages

# ---- literals and fields ---------------------------------------------------------

pbi_lit <- function(x) {
  value <- if (is.character(x)) paste0("'", x, "'") else if (is.logical(x)) tolower(x) else if (is.integer(x)) paste0(x, "L") else paste0(x, "D")
  list(expr = list(Literal = list(Value = value)))
}

pbi_color <- function(hex) list(solid = list(color = pbi_lit(hex)))

f_col <- function(entity, prop) {
  list(Column = list(Expression = list(SourceRef = list(Entity = entity)), Property = prop))
}

f_meas <- function(entity, prop) {
  list(Measure = list(Expression = list(SourceRef = list(Entity = entity)), Property = prop))
}

# Aggregation functions in PBIR: 0 = Sum, 1 = Average
f_agg <- function(entity, prop, fn = 0) {
  list(Aggregation = list(Expression = f_col(entity, prop), Function = fn))
}

# `label` becomes the displayName shown in axes, legends and table headers
proj_col <- function(entity, prop, active = NULL, label = NULL) {
  p <- list(field = f_col(entity, prop), queryRef = paste0(entity, ".", prop),
            nativeQueryRef = prop, displayName = label %||% prop)
  if (!is.null(active)) p$active <- active
  p
}

proj_meas <- function(entity, prop, label = NULL) {
  list(field = f_meas(entity, prop), queryRef = paste0(entity, ".", prop),
       nativeQueryRef = prop, displayName = label %||% prop)
}

proj_sum <- function(entity, prop, label = NULL) {
  list(field = f_agg(entity, prop, 0), queryRef = paste0("Sum(", entity, ".", prop, ")"),
       nativeQueryRef = label %||% paste("Sum of", prop), displayName = label %||% prop)
}

proj_avg <- function(entity, prop, label = NULL) {
  list(field = f_agg(entity, prop, 1), queryRef = paste0("Average(", entity, ".", prop, ")"),
       nativeQueryRef = label %||% paste("Average of", prop), displayName = label %||% prop)
}

role <- function(...) list(projections = list(...))

# ---- visuals -----------------------------------------------------------------------

palette_main <- "#1F6F8B"
palette_accent <- "#E4572E"

title_object <- function(text) {
  list(title = list(list(properties = list(
    show = pbi_lit(TRUE), text = pbi_lit(text),
    fontSize = pbi_lit(14), bold = pbi_lit(TRUE)
  ))))
}

make_visual <- function(name, x, y, w, h, visual_type, query_state, title = NULL,
                        objects = NULL, sort = NULL, z = 0) {
  query <- list(queryState = query_state)
  if (!is.null(sort)) {
    query$sortDefinition <- list(sort = list(list(field = sort$field, direction = sort$direction)))
  }
  visual <- list(visualType = visual_type, query = query, drillFilterOtherVisuals = TRUE)
  if (!is.null(objects)) visual$objects <- objects
  if (!is.null(title)) visual$visualContainerObjects <- title_object(title)
  list(
    `$schema` = "https://developer.microsoft.com/json-schemas/fabric/item/report/definition/visualContainer/2.12.0/schema.json",
    name = name,
    position = list(x = x, y = y, z = z, height = h, width = w, tabOrder = z),
    visual = visual
  )
}

solid_fill <- function(hex) {
  list(dataPoint = list(list(properties = list(fill = pbi_color(hex)))))
}

v_card <- function(name, x, y, w, h, entity, measure, title) {
  make_visual(name, x, y, w, h, "card", list(Values = role(proj_meas(entity, measure))),
              title = title,
              objects = list(categoryLabels = list(list(properties = list(show = pbi_lit(FALSE))))))
}

v_slicer <- function(name, x, y, w, h, entity, prop, title) {
  make_visual(
    name, x, y, w, h, "slicer", list(Values = role(proj_col(entity, prop, active = TRUE))),
    title = title,
    objects = list(data = list(list(properties = list(mode = pbi_lit("Dropdown")))))
  )
}

v_column <- function(name, x, y, w, h, category, values, title, color = palette_main,
                     sort = NULL, type = "clusteredColumnChart", axis_start = NULL,
                     axis_end = NULL, precision = 2L) {
  objects <- c(
    solid_fill(color),
    list(labels = list(list(properties = list(show = pbi_lit(TRUE), labelPrecision = pbi_lit(precision)))))
  )
  if (!is.null(axis_start)) {
    objects$valueAxis <- list(list(properties = list(
      start = pbi_lit(axis_start), end = pbi_lit(axis_end), showAxisTitle = pbi_lit(FALSE)
    )))
  }
  make_visual(name, x, y, w, h, type,
              list(Category = role(category), Y = do.call(role, values)),
              title = title, objects = objects, sort = sort)
}

v_line <- function(name, x, y, w, h, category, values, title) {
  make_visual(name, x, y, w, h, "lineChart",
              list(Category = role(category), Y = do.call(role, values)), title = title)
}

v_scatter <- function(name, x, y, w, h, detail, x_proj, y_proj, title, series = NULL) {
  qs <- list(Category = role(detail), X = role(x_proj), Y = role(y_proj))
  if (!is.null(series)) qs$Series <- role(series)
  make_visual(name, x, y, w, h, "scatterChart", qs, title = title)
}

v_table <- function(name, x, y, w, h, columns, title, sort = NULL) {
  make_visual(name, x, y, w, h, "tableEx", list(Values = do.call(role, columns)),
              title = title, sort = sort,
              objects = list(total = list(list(properties = list(totals = pbi_lit(FALSE))))))
}

v_text <- function(name, x, y, w, h, text, size = 12, bold = FALSE, color = "#333333") {
  list(
    `$schema` = "https://developer.microsoft.com/json-schemas/fabric/item/report/definition/visualContainer/2.12.0/schema.json",
    name = name,
    position = list(x = x, y = y, z = 0, height = h, width = w, tabOrder = 0),
    visual = list(
      visualType = "textbox",
      objects = list(general = list(list(properties = list(paragraphs = list(list(
        textRuns = list(list(
          value = text,
          textStyle = list(fontSize = paste0(size, "pt"), fontWeight = if (bold) "bold" else "normal",
                           color = color)
        ))
      )))))),
      drillFilterOtherVisuals = TRUE
    )
  )
}

# ---- pages and report ------------------------------------------------------------------

make_page_json <- function(page_name, display_name, width = 1280, height = 720,
                           background = "#F4F6F8") {
  list(
    `$schema` = "https://developer.microsoft.com/json-schemas/fabric/item/report/definition/page/2.1.0/schema.json",
    name = page_name, displayName = display_name, displayOption = "FitToPage",
    height = height, width = width,
    objects = list(background = list(list(properties = list(
      color = pbi_color(background), transparency = pbi_lit(0)
    ))))
  )
}

write_json_pretty <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
}

# pages: named list; each element is list(title =, visuals = list of visual lists)
write_report <- function(report_dir, pages, template_dir) {
  def_dir <- file.path(report_dir, "definition")
  dir.create(def_dir, recursive = TRUE, showWarnings = FALSE)

  write_json_pretty(
    list(version = "4.0", datasetReference = list(byPath = list(path = "../sports_stats.SemanticModel"))),
    file.path(report_dir, "definition.pbir")
  )

  # report-level files are reused from the original report (theme, settings, schema versions)
  file.copy(file.path(template_dir, "definition", "report.json"), file.path(def_dir, "report.json"),
            overwrite = TRUE)
  file.copy(file.path(template_dir, "definition", "version.json"), file.path(def_dir, "version.json"),
            overwrite = TRUE)
  file.copy(file.path(template_dir, "StaticResources"), report_dir, recursive = TRUE)

  page_ids <- names(pages)
  write_json_pretty(
    list(`$schema` = "https://developer.microsoft.com/json-schemas/fabric/item/report/definition/pagesMetadata/1.1.0/schema.json",
         pageOrder = as.list(page_ids), activePageName = page_ids[1]),
    file.path(def_dir, "pages", "pages.json")
  )

  purrr::iwalk(pages, \(pg, pid) {
    page_dir <- file.path(def_dir, "pages", pid)
    write_json_pretty(make_page_json(pid, pg$title), file.path(page_dir, "page.json"))
    purrr::walk(pg$visuals, \(v) {
      write_json_pretty(v, file.path(page_dir, "visuals", v$name, "visual.json"))
    })
  })
  invisible(report_dir)
}
