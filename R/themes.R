# set default colour scale, font, axes

ggplot2::set_theme(
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Sarabun"),
      panel.grid.major.y = element_blank(),
      axis.ticks.x.bottom = element_line(colour = "#aaa"),
      panel.grid.major.x = element_line(colour = "#fff"),
      panel.grid.minor.x = element_blank(),
      title = element_text(face = "bold"),
      panel.ontop = TRUE
    )
)

set_fill_scale <- function(name = NULL) {
  ggplot2::scale_fill_manual(
    name = name,
    values = c("#eeeeee", "#1f77b4", "#2ca02c", "#f5d41d", "#ff7f0e", "#d62728")
  )
}

set_fill_threshold_scale <- function(name = NULL, threshold = 1) {
  threshold_colour = c(
    "0" = "#1f77b4",
    "0.01" = "#2ca02c",
    "0.1" = "#f5d41d",
    "1" = "#ff7f0e",
    "10" = "#d62728"
  )
  ggplot2::scale_fill_manual(
    name = name,
    values = threshold_colour[[threshold]],
    guide = "none" # don't show a separate legend for this
  )
}

scale_x_continuous_probability <- function(limits = c(0, 1)) {
  ggplot2::scale_x_continuous(
    labels = scales::percent,
    breaks = c(0, 0.2, 0.40, 0.60, 0.80, 1.00),
    limits = limits
  )
}

scale_y_discrete_months <- function() {
  ggplot2::scale_y_discrete()
}
