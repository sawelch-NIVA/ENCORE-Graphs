multiple_stressors_data <- data_long_pretty |>
    filter(
        # include only all stressors grouped together
        stressor_group == "all",
        # don't include 1 - p_gt1, only p_gt and p_exceedence for SumSumRQ
        ((exceedence_boolean &
            sum_operation != "SumSumRQ") |
            (sum_operation == "SumSumRQ" & !is.na(RQ_range)))
    )


# 0 ~ 0 - 0.01, 1 ~ 0.01 ~ 0.1, 2 ~ 0.1 - 1, 3 ~ 1-10, 4 ~ 10 - Inf
thresholds <- rq_level_ranges |>
    filter(RQ_lower_bound != 0) |>
    pull(RQ_lower_bound)

# Loop over each threshold
for (threshold in thresholds) {
    multiple_stressors_data_gt_n <- multiple_stressors_data |>
        filter(
            sum_operation == "SumSumRQ" |
                sum_operation_threshold == threshold
        ) |>
        group_by(Month_abb, sum_operation, rbd_name, sum_operation_threshold) |>
        mutate(
            Probability_perc_scaled = case_when(
                sum_operation == "SumSumRQ" ~ Probability_perc /
                    sum(Probability_perc) *
                    100,
                .default = Probability_perc
            ),
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
            RQ_range = case_when(
                !is.na(RQ_range) ~ RQ_range,
                TRUE ~ rq_level_ranges |>
                    filter(RQ_lower_bound == threshold) |>
                    pull(RQ_range)
            )
        ) |>
        ungroup()

    p <- multiple_stressors_data_gt_n |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_scaled,
            fill = RQ_range,
        )) +
        geom_col() +
        facet_grid(
            cols = vars(sum_operation),
            rows = vars(rbd_name),
            switch = "y"
        ) +
        scale_x_continuous(breaks = c(0, 50, 100)) +
        scale_y_discrete(breaks = c("Jan", "Apr", "Jul", "Oct")) +
        labs(
            x = "Probability of Exceedence (%) ",
            y = "",
            title = glue(
                "Probability that a Risk metric exceeds {threshold}, by month and river basin"
            ),
            subtitle = "All stressors, Belgium (modelled data)"
        ) +
        set_colour_scale(name = "RQ Range") +
        theme(
            strip.text = element_markdown(),
            strip.placement = "outside"
        )

    filename <- glue("images/fig3_multiple_risk_metrics_gt_{threshold}.png")
    ggsave(filename = filename, plot = p, width = 21, height = 30, units = "cm")
    message(glue("Saved {filename}"))
}
