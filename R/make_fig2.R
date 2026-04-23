# Check: interval probabilities sum to 100% per node per month/RBD
check_sums <- data_long_pretty_merged |>
    filter(comparison_operation == "interval") |>
    summarise(
        total = sum(Probability_perc_merged),
        .by = c(
            Month_abb,
            rbd_name,
            stressor_group,
            stressor_code,
            sum_operation
        )
    ) |>
    filter(abs(total - 100) > 0.5)
stopifnot(nrow(check_sums) == 0)

grouped_stressors_data <- data_long_pretty_merged |>
    filter(
        sum_operation == "SumRQ",
        stressor_group != "all",
        comparison_operation == "interval"
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
    left_join(stressor_group_icons, by = "stressor_group") |>
    mutate(
        rbd_name = glue("**{rbd_name}**"),
        group_and_n = glue(
            "<img src='{icon_path}' width='10' vertical-align='bottom'/> {stressor_group_name} (n = {stressor_sample_size})"
        ),
        rbd_and_group_md = glue(
            "{facet_letter}) **{rbd_name}** ({stressor_group_name} (*n = {stressor_sample_size}*))"
        )
    )

stopifnot(nrow(grouped_stressors_data) > 0)


p <- grouped_stressors_data |>
    ggplot(aes(
        y = fct_rev(Month_abb),
        x = Probability_perc_merged,
        fill = RQ_range_merged
    )) +
    geom_col(position = "fill", width = geom_col_width) +
    # facet_wrap(
    #     facets = vars(rbd_and_group_md),
    #     nrow = 5,
    #     ncol = 3,
    #     dir = "h"
    # ) +
    facet_grid(
        rows = vars(rbd_name),
        cols = vars(group_and_n),
        axes = "all_x",
        switch = "y"
    ) +
    scale_x_continuous_probability(limits = NULL) +
    scale_y_discrete_months() +
    labs(
        x = "Probability RQ in Interval",
        y = NULL,
        tag = "a",
        title = glue(
            "Probability distributions for Sum of Risk Quotient by month and river basin"
        ),
        subtitle = "All stressors, Belgium (predicted)"
    ) +
    set_fill_scale(name = "RQ interval") +
    guides(fill = guide_legend(nrow = 1)) +
    theme(
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.placement = "outside",
        strip.text.y.left = element_markdown(size = 12),
        legend.position = "bottom"
    )

filename <- "images/fig2_grouped_stressors.png"
ggsave(filename = filename, plot = p, width = 24, height = 30, units = "cm")
message(paste0("saved ", filename))
