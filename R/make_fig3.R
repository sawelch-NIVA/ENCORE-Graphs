mixture_stressors_data <- data_long_pretty |>
    filter(
        # include only all stressors grouped together
        stressor_group == "all",
        # don't include 1 - p_gt1, only p_gt and p_exceedence for SumSumRQ
        (exceedence_boolean | sum_operation == "SumSumRQ")
    )

# why do we have so many damn values?

# group_by(stressor_group, rbd_name) |>
# mutate(
#     facet_letter = letters[cur_group_id()],
# ) |>
# mutate(
#     rbd_and_group_md = glue(
#         "{facet_letter}) **{rbd_name}**, {stressor_group_name}s"
#     )
# )

mixture_stressors_data |>
    ggplot(aes(
        y = fct_rev(Month_abb),
        x = Probability_perc_scaled,
        fill = RQ_range
    )) +
    geom_col() +
    facet_grid(
        cols = vars(sum_operation),
        rows = vars(rbd_name)
    ) +
    scale_x_continuous(breaks = c(0, 50, 100)) +
    scale_y_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
    labs(
        x = "Probability of Exceedence (%) ",
        y = "",
        title = glue(
            "Probability distributions for Sum of Risk Quotient by month and river basin"
        ),
        subtitle = "All stressors, Belgium (modelled data)"
    ) +
    theme(strip.text = element_markdown(halign = 0)) +
    set_colour_scale(name = "RQ Range") +
    theme(
        text = element_text(size = 12, family = "Sarabun"),
        panel.grid.major = element_blank(),
        title = element_text(face = "bold")
    )

filename <- "images/fig2_grouped_stressors.png"
ggsave(filename = filename, plot = p, width = 21, height = 30, units = "cm")
