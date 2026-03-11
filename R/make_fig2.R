grouped_stressors_data <- data_long_pretty |>
    filter(
        stressor_type == "SumRQ",
        stressor_group != "all",
        RQ_operation == "level"
    ) |>
    group_by(stressor_group, rbd_name) |>
    mutate(
        facet_letter = letters[cur_group_id()],
        stressor_sample_size = recode_values(
            stressor_group,
            "herbi" ~ 9,
            "fungi" ~ 5,
            "insec" ~ 2
        )
    ) |>
    mutate(
        rbd_and_group_md = glue(
            "{facet_letter}) **{rbd_name}**, {stressor_group_name}s"
        )
    )

p <- grouped_stressors_data |>
    ggplot(aes(y = fct_rev(Month_abb), x = Probability_perc, fill = RQ_range)) +
    geom_col() +
    facet_wrap(
        facets = vars(rbd_and_group_md),
        nrow = 5,
        ncol = 3,
        dir = "v"
    ) +
    scale_x_continuous(breaks = c(0, 50, 100)) +
    scale_y_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
    labs(
        x = "Probability (%) RQ in Range",
        y = "",
        title = glue(
            "Probability Distributions for Sum of Risk Quotient By Month and River Basin"
        ),
        subtitle = "All Stressors, Belgium (Modelled Data)"
    ) +
    theme(strip.text = element_markdown(halign = 0)) +
    set_colour_scale(name = "RQ Range") +
    theme(
        text = element_text(size = 12, family = "Sarabun"),
        panel.grid.major = element_blank(),
        title = element_text(face = "bold")
    )

filename <- "images/fig2_grouped_stressors.png"
ggsave(filename = filename, plot = p, width = 24, height = 30, units = "cm")
