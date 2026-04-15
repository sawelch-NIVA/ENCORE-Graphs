# set default colour scale, font, axes

ggplot2::set_theme(
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "Sarabun"),
      panel.grid.major = element_blank(),
      title = element_text(face = "bold")
    )
)

set_colour_scale <- function(name = NULL) {
  ggplot2::scale_fill_manual(
    name = name,
    values = c("#eeeeee", "#1f77b4", "#2ca02c", "#f5d41d", "#ff7f0e", "#d62728")
  )
}

scale_x_continuous_probability <- function() {
  ggplot2::scale_x_continuous(n.breaks = 5, labels = scales::percent)
}

scale_y_discrete_months <- function() {
  ggplot2::scale_y_discrete()
}
