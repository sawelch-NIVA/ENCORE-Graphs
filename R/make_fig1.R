individual_stressors <- data_long_pretty |> filter(!is.na(stressor_code))

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
      comparison_operation == "level"
    )

  p <- plot_data |>
    ggplot(aes(
      y = fct_rev(Month_abb),
      x = Probability_perc_scaled,
      fill = RQ_range
    )) +
    geom_col() +
    facet_wrap(vars(fct_inorder(stressor_name_group_md))) +
    scale_x_continuous(breaks = c(0, 50, 100)) +
    scale_y_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
    labs(
      x = "Probability (%) RQ in Range",
      y = "",
      title = glue(
        "Probability distributions for Risk Quotient by stressor and month"
      ),
      subtitle = glue("{rbd_full_name}, Belgium (predicted)")
    ) +
    theme(strip.text = element_markdown(hjust = 0)) +
    set_colour_scale(name = "RQ Range") +
    theme(
      text = element_text(size = 12, family = "Sarabun"),
      panel.grid.major = element_blank(),
      title = element_text(face = "bold"),
      legend.position = c(0.85, 0.08),
      legend.justification = c(1, 0.2),
      legend.margin = margin(5, 5, 5, 5),
      legend.key.height = unit(0.5, "cm")
    )

  filename <- glue(
    "images/fig1_{str_to_lower(str_replace_all(rbd_code, '_', '-'))}.png"
  )
  ggsave(filename = filename, plot = p, width = 30, height = 21, units = "cm")
})
