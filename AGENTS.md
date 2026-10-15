# Project Instructions: R Data Science & Analytics

## 1. Project Architecture & Standards
* **Environment Management:** Install missing packages using `pak::pak` in the user library (`pak::pak("package_name")`). 
* **Code Style:** Follow the [tidyverse style guide](https://style.tidyverse.org/). Use strict `snake_case` (all lowercase) for variable, column, and function names (e.g., `mean_density`, `calculate_density()`).
* **Paths:** Always use relative paths (e.g., `"data/raw"`, `"data/processed"`, `"figs/"`, `"tabs/"`). Never hardcode absolute local paths (`C:/...` or `/home/...`).
* Always give preference for using functions and packages from the tidyverse, specially for manipulating dataframes.
* All scripts will be run inside a IDE. Dont write R script with inline arguments for using from the terminal

## 2. Directory Structure Conventions
Keep tasks scoped to their designated directories:
- Root (`./`) — All primary modular execution scripts (`01_...R`, `02_...R`, `00_run_all.R`).
- `R/` — Reusable custom helper functions and modular utilities (e.g., `R/utils_ai.R`). All functions must be saved on its own file here, aggregated by topic (e.g. modelling.R, plots.R, ..)
- `data/raw/` — Read-only primary datasets. Never modify or overwrite raw files programmatically.
- `data/processed/` — Cleaned intermediate `.rds` or `.parquet` files.
- `tabs/` — Exported econometric regression tables (`.rds`, `.tex`, `.html`) and tidy model summaries (`broom::tidy()`).
- `figs/` — Rendered figures and plots (`.png`).

## 3. Console Reporting & User Messages
* **CLI Package:** Always use package `cli` (`cli::cli_h1()`, `cli::cli_alert_info()`, `cli::cli_alert_success()`, `cli::cli_bullets()`) for terminal console reporting instead of `base::cat()` or `print()`.

## 4. Data Wrangling & Manipulation Rules
* **column names**: Use `janitor::clean_names()` to sanitize all column names from raw data
* **Modern Syntax:** Prefer `dplyr` / `tidyr` or `data.table` for high-performance memory operations. 
* **Pipes:** Standardize strictly on the native pipe `|>` (R 4.1+) over `%>%`.
* **DuckDB Engine:** Use `DBI::dbConnect(duckdb::duckdb())` and `DBI::dbGetQuery()` / `DBI::dbExecute()` for out-of-core SQL operations on large `.parquet` datasets.
* **Explicit Scoping:** Disambiguate functions using `package::function()` notation (e.g., `dplyr::filter()`, `DBI::dbGetQuery()`).

## 5. Modeling & Econometrics
* **Fixed Effects Regressions:** Use `fixest::feols()` for high-dimensional fixed-effects models (`y ~ x | fe1 + fe2`) with appropriate standard error clustering (`cluster = ~group_id`).
* **Tidy Summaries:** Use `broom::tidy()` or `broom::glance()` to extract clean data frame summaries from model outputs.
* use package `texreg` for creating estimation tables in latex.
* **Seed Control:** Set explicit random seeds (`set.seed(...)`) at the top of any script involving stochastic processes or bootstrapping.

## 6. Output & Reproducibility
* **Graphics:** Use `ggplot2` for all visualizations. Always specify explicit canvas dimensions and resolution (`dpi = 300`) when invoking `ggplot2::ggsave()`.
* **Master Execution:** Maintain an explicit execution order top-to-bottom across root scripts (`01_...R`, `02_...R`, etc.) or via a master runner (`00_run_all.R`).
