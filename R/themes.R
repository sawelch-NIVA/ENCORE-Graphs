# set default colour scale, font, axes
set_fill_scale <- function(name = NULL, drop = NULL) {
  ggplot2::scale_fill_manual(
    name = name,
    drop = drop,
    values = c(
      "0 - 0" = "#eeeeee",
      "0 - 0.01" = "#1f77b4",
      "0.01 - 0.1" = "#2ca02c",
      "0.1 - 1" = "#f5d41d",
      "1 - 10" = "#ff7f0e",
      "10 - Inf" = "#d62728"
    )
  )
}

set_manuscript_theme <- function() {
  ggplot2::set_theme(
    theme_minimal() +
      theme(
        text = element_text(size = 12, family = "Sarabun"),
        panel.grid.major.y = element_blank(),
        axis.ticks.x.bottom = element_line(colour = "#aaa"),
        panel.grid.major.x = element_line(colour = "#8e8b8b"),
        panel.grid.minor.x = element_line(colour = "#a6a6a6"),
        title = element_text(face = "bold")
        # panel.ontop = TRUE
      )
  )
}

set_poster_theme <- function() {
  ggplot2::set_theme(
    theme_minimal() +
      theme(
        text = element_text(size = 12, family = "Sarabun"),
        panel.grid.major.y = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        panel.grid.major.x = element_line(
          colour = "#8e8b8b",
          linewidth = rel(2)
        ),
        panel.grid.minor.x = element_line(
          colour = "#a6a6a6",
          linewidth = rel(2)
        ),
        title = element_text(face = "bold")
        # panel.ontop = TRUE
      )
  )
}

set_fill_threshold_scale <- function(
  name = NULL,
  threshold = 1,
  lighten = 0 # lighten the bars corresponding to a 50% reduction in alpha (but keep opaque)
) {
  threshold_colour = c(
    "0" = "#1f77b4",
    "0.01" = "#2ca02c",
    "0.1" = "#f5d41d",
    "1" = "#ff7f0e",
    "10" = "#d62728"
  )

  ggplot2::scale_fill_manual(
    name = name,
    values = if (lighten != 0) {
      colorspace::lighten(threshold_colour[[threshold]], lighten)
    } else {
      threshold_colour[[threshold]]
    },
    guide = "none" # don't show a separate legend for this
  )
}

scale_x_continuous_probability <- function(limits = c(0, 1), expand = TRUE) {
  ggplot2::scale_x_continuous(
    labels = scales::percent,
    breaks = c(0, 0.2, 0.40, 0.60, 0.80, 1.00),
    limits = limits,
    expand = expand
  )
}

scale_y_discrete_months <- function() {
  ggplot2::scale_y_discrete()
}

# Single-letter month axis (J F M A M J J A S O N D — duplicates are intentional)
scale_y_poster_months <- function() {
  scale_y_discrete(labels = \(x) substr(x, 1, 1))
}

# Draws a single grey outline around the combined bars that fall above `threshold`
# in the SumSumRQ interval stacked bar chart. `data` should already be filtered
# to sum_operation == "SumSumRQ" & comparison_operation == "interval".
geom_intervals_outlined <- function(
  data,
  threshold,
  colour = "#222",
  linewidth = 0.5,
  width = 0.9
) {
  outlined_ranges <- if (threshold == 0.1) {
    c("0.1 - 1", "1 - 10", "10 - Inf")
  } else {
    c("1 - 10", "10 - Inf")
  }

  rect_data <- data |>
    dplyr::filter(RQ_range_merged %in% outlined_ranges) |>
    dplyr::summarise(
      prop = sum(Probability_perc_merged) / 100,
      .by = Month_abb
    ) |>
    dplyr::mutate(
      y_pos = as.numeric(forcats::fct_rev(Month_abb)),
      ymin = y_pos - width / 2,
      ymax = y_pos + width / 2,
      xmin = 0,
      xmax = prop
    )

  ggplot2::geom_rect(
    data = rect_data,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    colour = colour,
    linewidth = linewidth,
    inherit.aes = FALSE
  )
}
