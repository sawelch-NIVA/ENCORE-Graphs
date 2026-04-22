# Check: interval probabilities sum to 100% per node per month/RBD
check_sums <- data_long_pretty_merged |>
  filter(comparison_operation == "interval") |>
  summarise(
    total = sum(Probability_perc_merged),
    .by = c(Month_abb, rbd_name, stressor_group, stressor_code, sum_operation)
  ) |>
  filter(abs(total - 100) > 0.5)
stopifnot(nrow(check_sums) == 0)

individual_stressors <- data_long_pretty_merged |> filter(!is.na(stressor_code))

# Get unique RBD names
unique_rbds <- individual_stressors |>
  distinct(RBD, rbd_name) |>
  arrange(rbd_name)

# Generate plots for each RBD
walk(seq_len(nrow(unique_rbds)), function(i) {
  rbd_code <- unique_rbds$RBD[i]
  rbd_full_name <- unique_rbds$rbd_name[i]

  plot_data <- individual_stressors |>
    filter(
      RBD == rbd_code,
      comparison_operation == "interval"
    )

  stopifnot(nrow(plot_data) > 0)

  p <- plot_data |>
    ggplot(aes(
      y = fct_rev(Month_abb),
      x = Probability_perc_merged,
      fill = RQ_range_merged
    )) +
    geom_col(position = "fill", width = geom_col_width) +
    facet_wrap(vars(fct_inorder(stressor_name_group_md)), axes = "all_x") +
    # Invisible points just to register the shape aesthetic
    geom_point(
      aes(shape = stressor_group),
      x = 0,
      size = 0,
      alpha = 0
    ) +
    scale_shape_manual(
      name = "Group",
      values = c("herbi" = 15, "fungi" = 16, "insec" = 17), # dummy shapes
      labels = c(
        "herbi" = "🌿 Herbicide",
        "fungi" = "🍄 Fungicide",
        "insec" = "🪲 Insecticide"
      ),
      guide = guide_legend(
        override.aes = list(size = 0, alpha = 0) # hide the actual points
      )
    ) +
    scale_x_continuous_probability(limits = NULL) +
    scale_y_discrete_months() +
    labs(
      x = "Probability RQ in Interval",
      y = "Month",
      title = glue(
        "Probability distributions for Risk Quotient by stressor and month"
      ),
      subtitle = glue("{rbd_full_name}, Belgium (predicted)")
    ) +
    set_fill_scale(name = "RQ interval") +
    theme(
      legend.position = c(0.95, 0.05),
      legend.box = "horizontal",
      legend.justification = c(1, 0.2),
      legend.margin = margin(5, 5, 5, 5),
      legend.key.height = unit(0.5, "cm"),
      strip.text = element_markdown(hjust = 0)
    )

  filename <- glue(
    "images/fig1_{str_to_lower(str_replace_all(rbd_code, '_', '-'))}.png"
  )
  ggsave(filename = filename, plot = p, width = 30, height = 21, units = "cm")
  message(paste0("saved ", filename))
})
