# Plots (ggplot2)

plot_hist_ratio <- function(data) {
  m <- mean(data$goals_xg)
  md <- stats::median(data$goals_xg)
  s <- stats::sd(data$goals_xg)
  ggplot2::ggplot(data, ggplot2::aes(goals_xg)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 30, fill = "grey80", color = "white") +
    ggplot2::geom_density(linewidth = 0.8) +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = m, sd = s),
                           linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 1, color = "red") +
    ggplot2::geom_vline(xintercept = m, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = md, linetype = "dotdash") +
    ggplot2::labs(
      x = "Goals / xG", y = "Density",
      subtitle = "Red: ratio = 1 | dotted: mean | dot-dash: median | dashed: fitted normal"
    )
}

plot_qq_ratio <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(sample = goals_xg)) +
    ggplot2::stat_qq(alpha = 0.5) +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(x = "Theoretical quantiles (normal)", y = "Sample quantiles")
}

plot_box_ratio <- function(data, group_var, rotate = FALSE) {
  p <- ggplot2::ggplot(data, ggplot2::aes(.data[[group_var]], goals_xg)) +
    ggplot2::geom_boxplot(fill = "grey90") +
    ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    ggplot2::labs(x = NULL, y = "Goals / xG")
  if (rotate) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  p
}

plot_scatter_xg_goals <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(xg, goals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "black", formula = y ~ x) +
    ggplot2::labs(x = "Season xG", y = "Season goals")
}

# Distribution fit plots (fitdistrplus uses base R graphics)
save_fit_plots <- function(fits, x, tag) {
  grDevices::png(file.path("figs", paste0("fit_plots_", tag, ".png")),
                 width = 9, height = 7, units = "in", res = 300)
  old_par <- graphics::par(mfrow = c(2, 2))
  fitdistrplus::denscomp(fits, legendtext = names(fits))
  fitdistrplus::qqcomp(fits, legendtext = names(fits))
  fitdistrplus::cdfcomp(fits, legendtext = names(fits))
  fitdistrplus::ppcomp(fits, legendtext = names(fits))
  graphics::par(old_par)
  grDevices::dev.off()

  grDevices::png(file.path("figs", paste0("cullen_frey_", tag, ".png")),
                 width = 6, height = 5, units = "in", res = 300)
  fitdistrplus::descdist(x, boot = 500)
  grDevices::dev.off()
  cli::cli_alert_success("Distribution fit figures saved ({tag})")
}
