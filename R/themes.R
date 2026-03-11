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
    values = c("#1f77b4", "#2ca02c", "#f5d41d", "#ff7f0e", "#d62728")
  )
}
