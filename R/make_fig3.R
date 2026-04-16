# Check: interval probabilities sum to 100% per node per month/RBD
check_sums <- data_long_pretty_merged |>
  filter(comparison_operation == "interval") |>
  summarise(
    total = sum(Probability_perc_merged),
    .by = c(Month_abb, rbd_name, stressor_group, stressor_code, sum_operation)
  ) |>
  filter(abs(total - 100) > 0.5)
stopifnot(nrow(check_sums) == 0)

# Set chosen thresholds
fig3_ranges <- c(0.1, 1)
# Set chosen location
fig3_rbd <- c("MAAS VL")

# Figure 3
# TODO: Add letter tags per facet
# TODO: Use BEEMAAS_VL as example, make 3 (group RQ metric) x 2 (>0.1 and 1) grid
# TODO: Use colour instead of fill for CA + IA and IA bars <-

multiple_stressors_data <- data_long_pretty_merged |>
    filter(
        # include only all stressors grouped together
        stressor_group == "all",
        # don't include 1 - p_gt1, only p_gt and p_exceedence for SumSumRQ
        ((exceedence_boolean &
            sum_operation != "SumSumRQ") |
            (sum_operation == "SumSumRQ" & !is.na(RQ_range_merged)))
    ) |>
    # get the lower limit of the range for easy grouping
    mutate(
        RQ_range_merged_threshold = str_extract(
            RQ_range_merged,
            pattern = "[0-9.]+$"
        ) |>
            as.numeric()
    ) |>
    select(
        -c(
            RBD,
            stressor_group,
            stressor_group_name,
            stressor_name_group_md,
            stressor_code,
            stressor_name
        )
    )

# filter to case study
multiple_stressors_data <- multiple_stressors_data |>
    filter(rbd_name %in% fig3_rbd)

stopifnot(nrow(multiple_stressors_data) > 0)

# Each month should have
#   6 (P(SumSumRQ ∈ interval)) +
#   1 (P(SumRQ > threshold)) +
#   1 P(AnyRQ > threshold)
# = 8 data points * 2 thresholds = 16

# Loop over each threshold
for (threshold in fig3_ranges) {
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
            x = "Probability of Exceedence",
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
