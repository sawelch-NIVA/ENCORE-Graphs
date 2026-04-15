# Set chosen thresholds
fig3_ranges <- c(0.1, 1)
# Set chosen location
fig3_rbd <- c("EMAAS VL")

multiple_stressors_data <- data_long_pretty_merged |>
    filter(
        # include only all stressors grouped together
        stressor_group == "all",
        # don't include 1 - p_gt1, only p_gt and p_exceedence for SumSumRQ
        ((exceedence_boolean &
            sum_operation != "SumSumRQ") |
            (sum_operation == "SumSumRQ" & !is.na(RQ_range_merged))),
        rbd_name == fig3_rbd,
        RQ_range_merged %in% fig3_thresholds
    )

stopifnot(nrow(multiple_stressors_data) > 0)


# Loop over each threshold
for (threshold in fig3_thresholds) {
    multiple_stressors_data_gt_n <- multiple_stressors_data |>
        filter(
            sum_operation == "SumSumRQ" |
                sum_operation_threshold == threshold
        ) |>
        group_by(Month_abb, sum_operation, rbd_name, sum_operation_threshold) |>
        mutate(
            sum_operation = replace_values(
                sum_operation,
                "SumSumRQ" ~ glue(
                    "**Concentration Addition**<br>*SumSumRQ > {threshold}*"
                ),
                "SumRQ" ~ glue(
                    "**Concentration Addition<br>& Independent Action**<br>*Any SumRQ > {threshold}*"
                ),
                "Any_RQ" ~ glue(
                    "**Independent Action**<br>*Any RQ > {threshold}*"
                )
            ),
            RQ_range_merged = case_when(
                !is.na(RQ_range_merged) ~ RQ_range_merged,
                TRUE ~ rq_level_ranges |>
                    filter(RQ_range_merged == threshold) |>
                    pull(RQ_range_merged)
            )
        ) |>
        ungroup()

    p <- multiple_stressors_data_gt_n |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged,
            fill = RQ_range_merged,
        )) +
        geom_col() +
        facet_grid(
            cols = vars(sum_operation),
            rows = vars(rbd_name),
            switch = "y"
        ) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        labs(
            x = "Probability of Exceedence (%) ",
            y = "",
            title = glue(
                "Probability that a Risk metric exceeds {threshold}, by month and river basin"
            ),
            subtitle = "All stressors, Belgium (modelled data)"
        ) +
        set_colour_scale(name = "RQ Interval") +
        theme(
            strip.text = element_markdown(),
            strip.placement = "outside"
        )

    filename <- glue("images/fig3_multiple_risk_metrics_gt_{threshold}.png")
    ggsave(filename = filename, plot = p, width = 21, height = 30, units = "cm")
    message(glue("Saved {filename}"))
}
