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
multiple_stressors_data_cases <- multiple_stressors_data |>
    filter(rbd_name %in% fig3_rbd)

stopifnot(nrow(multiple_stressors_data) > 0)

# Each Case Study should have
#   6 (P(SumSumRQ ∈ interval)) +
#   1 (P(SumRQ > threshold) * 5 thresholds) +
#   1 (P(AnyRQ > threshold) * 5 thresholds)
#   * 12 months
# = 192 rows
stopifnot(nrow(multiple_stressors_data_cases) / length(fig3_rbd) == 192)


# Build one row (3 panels) per threshold, then stack rows with patchwork
make_threshold_row <- function(data, threshold) {
    # Left: SumSumRQ interval distribution — same stacked-bar style as fig1/fig2
    p_left <- data |>
        filter(
            sum_operation == "SumSumRQ",
            comparison_operation == "interval"
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged,
            fill = RQ_range_merged
        )) +
        geom_col(position = "fill") +
        # need to set limits = NULL for this graph or lots of values get culled
        scale_x_continuous_probability(limits = NULL) +
        scale_y_discrete_months() +
        set_fill_scale(name = "RQ Interval") +
        labs(
            x = "P(SumSumRQ ∈ interval)",
            y = NULL,
            subtitle = glue(
                "**Concentration Addition**<br>"
            )
        ) +
        theme(plot.subtitle = element_markdown(hjust = 0.5))

    # Middle: P(Any SumRQ > threshold) — single value per month, colour not fill
    p_mid <- data |>
        filter(
            sum_operation == "SumRQ",
            comparison_operation == "exceeds",
            sum_operation_threshold == threshold
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged / 100,
            colour = as.character(threshold)
        )) +
        geom_col(fill = NA) +
        set_colour_threshold_scale(threshold = as.character(threshold)) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        labs(
            x = glue("P(Any SumRQ > {threshold})"),
            y = NULL,
            subtitle = glue(
                "**Concentration Addition<br>& Independent Action**<br>"
            )
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.subtitle = element_markdown(hjust = 0.5)
        )

    # Right: P(Any RQ > threshold) — same treatment as middle
    p_right <- data |>
        filter(
            sum_operation == "Any_RQ",
            comparison_operation == "exceeds",
            sum_operation_threshold == threshold
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged / 100,
            colour = as.character(threshold),
        )) +
        geom_col(fill = NA) +
        set_colour_threshold_scale(threshold = as.character(threshold)) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        labs(
            x = glue("P(Any RQ > {threshold})"),
            y = NULL,
            subtitle = glue("**Independent Action**<br>")
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.subtitle = element_markdown(hjust = 0.5)
        )

    return(list(p_left, p_mid, p_right))
}

p <- map(fig3_ranges, \(threshold) {
    make_threshold_row(multiple_stressors_data_cases, threshold)
}) |>
    list_flatten() |>
    wrap_plots(ncol = 3, nrow = 2, guides = "collect") +
    plot_annotation(
        title = glue(
            "Probability of exceedance by risk metric, {paste(fig3_rbd, collapse = ', ')}"
        ),
        subtitle = "All stressors, Belgium (modelled data)",
        tag_levels = "a",
        tag_suffix = ")"
    )

filename <- "images/fig3_multiple_risk_metrics.png"
ggsave(filename = filename, plot = p, width = 30, height = 24, units = "cm")
message(glue("Saved {filename}"))
