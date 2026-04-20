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

# Graphics presets
alpha <- 0.5

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
make_threshold_row <- function(data, threshold, start_letter) {
    letters_row <- letters[start_letter:(start_letter + 2)]
    # hacky, fragile way to add grey outlines around relevant ranges
    outline_bars_intervals <- if (threshold == 0.1) {
        c("0.1 - 1", "1 - 10", "10 - Inf")
    } else {
        c("1 - 10", "10 - Inf")
    }

    p_sumsum <- data |>
        filter(
            sum_operation == "SumSumRQ",
            comparison_operation == "interval"
        ) |>
        mutate(outline_bars = RQ_range_merged %in% outline_bars_intervals) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged,
            fill = RQ_range_merged,
            colour = outline_bars
        )) +
        geom_col(position = "fill") +
        scale_x_continuous_probability(limits = NULL) +
        scale_color_manual(
            values = c("TRUE" = "#777", "FALSE" = NA),
            na.translate = FALSE,
            guide = NULL
        ) +
        scale_y_discrete_months() +
        set_fill_scale(name = "RQ interval") +
        labs(
            x = "P(SumSumRQ ∈ interval)",
            y = NULL,
            title = glue("{letters_row[1]}) Concentration Addition (CA)")
        )

    p_anysum <- data |>
        filter(
            sum_operation == "SumRQ",
            comparison_operation == "exceeds",
            sum_operation_threshold == threshold
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged / 100,
            fill = as.character(threshold)
        )) +
        geom_col(colour = "#777", alpha = alpha) +
        set_fill_threshold_scale(threshold = as.character(threshold)) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        labs(
            x = glue("P(Any SumRQ > {threshold})"),
            y = NULL,
            title = glue(
                "{letters_row[2]}) CA + IA"
            )
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )

    p_any <- data |>
        filter(
            sum_operation == "Any_RQ",
            comparison_operation == "exceeds",
            sum_operation_threshold == threshold
        ) |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged / 100,
            fill = as.character(threshold),
        )) +
        geom_col(colour = "#888888", alpha = alpha) +
        set_fill_threshold_scale(threshold = as.character(threshold)) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        labs(
            x = glue("P(Any RQ > {threshold})"),
            y = NULL,
            title = glue("{letters_row[3]}) Independent Action (IA)")
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )

    return(list(p_sumsum, p_anysum, p_any))
}

p <- imap(fig3_ranges, \(threshold, i) {
    start_letter <- (i - 1) * 3 + 1
    make_threshold_row(multiple_stressors_data_cases, threshold, start_letter)
}) |>
    list_flatten() |>
    wrap_plots(
        ncol = 3,
        nrow = 2,
        guides = "collect",
        axis_titles = "keep"
    ) +
    plot_annotation(
        title = glue(
            "Probability of exceedance by risk metric, {paste(fig3_rbd, collapse = ', ')}"
        ),
        subtitle = "All stressors (n = 15), Belgium (modelled data)"
    )

filename <- "images/fig3_multiple_risk_metrics.png"
ggsave(filename = filename, plot = p, width = 30, height = 24, units = "cm")
message(glue("Saved {filename}"))
