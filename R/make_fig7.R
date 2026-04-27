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
fig7_ranges <- c(0.1, 1)
# Set chosen location
fig7_rbd <- c("BEMAAS_VL")

# Graphics presets
alpha <- 1

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
            stressor_group,
            stressor_group_name,
            stressor_name_group_md,
            stressor_code,
            stressor_name
        )
    )

# Get unique RBD names from the multiple stressors data
unique_rbds <- multiple_stressors_data |>
    distinct(RBD, rbd_name) |>
    arrange(rbd_name)

stopifnot(nrow(multiple_stressors_data) > 0)


# Build one row (3 panels) per threshold, then stack rows with patchwork
# Build one row (3 panels + row label) per threshold, then stack rows with patchwork
make_threshold_row <- function(data, threshold, start_letter) {
    letters_row <- letters[start_letter:(start_letter + 2)]

    # -- Row label grob ----
    row_label_grob <- textGrob(
        glue("RQ threshold = {threshold}"),
        rot = 90,
        gp = gpar(fontfamily = "Sarabun", fontsize = 13, fontface = "bold")
    )

    p_sumsum_data <- data |>
        filter(
            sum_operation == "SumSumRQ",
            comparison_operation == "interval"
        )

    p_sumsum <- p_sumsum_data |>
        ggplot(aes(
            y = fct_rev(Month_abb),
            x = Probability_perc_merged,
            fill = RQ_range_merged
        )) +
        geom_col(position = "fill", width = geom_col_width) +
        geom_intervals_outlined(p_sumsum_data, threshold) +
        scale_x_continuous_probability(limits = NULL) +
        scale_y_discrete_months() +
        set_fill_scale(name = "RQ interval") +
        labs(
            x = glue("Probability of SumSumRQ > {threshold}"),
            y = NULL,
            title = glue("{letters_row[1]}) Concentration Addition (CA)")
        ) +
        coord_cartesian(expand = FALSE) +
        theme(
            panel.border = element_rect(
                fill = NA,
                colour = "#777",
                linewidth = 1
            ),
            plot.margin = unit(c(10, 30, 0, 0), "pt")
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
        set_fill_threshold_scale(
            threshold = as.character(threshold),
            lighten = 0.2
        ) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        coord_cartesian(expand = FALSE) +
        labs(
            x = glue("Probability of any RQ > {threshold}"),
            y = NULL,
            title = glue("{letters_row[2]}) Independent Action (IA)")
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_rect(
                fill = NA,
                colour = "#777",
                linewidth = 1
            ),
            plot.margin = unit(c(0, 30, 0, 0), "pt")
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
        set_fill_threshold_scale(
            threshold = as.character(threshold),
            lighten = 0.2
        ) +
        scale_x_continuous_probability() +
        scale_y_discrete_months() +
        labs(
            x = glue("Probability of Any SumRQ > {threshold}"),
            y = NULL,
            title = glue("{letters_row[3]}) CA + IA")
        ) +
        coord_cartesian(expand = FALSE) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_rect(
                fill = NA,
                colour = "#777",
                linewidth = 1
            ),
            plot.margin = unit(c(0, 0, 0, 0), "pt")
        )

    # -- Combine label + 3 panels into a single row ----
    row_with_label <- wrap_plots(
        list(wrap_elements(full = row_label_grob), p_sumsum, p_any, p_anysum),
        ncol = 4,
        widths = c(1, 19, 19, 19) # narrow label column, equal plot columns
    )

    return(row_with_label)
}

# -- Generate plots for each RBD ----
walk(seq_len(nrow(unique_rbds)), function(i) {
    rbd_code <- unique_rbds$RBD[i]
    rbd_full_name <- unique_rbds$rbd_name[i]

    multiple_stressors_data_cases <- multiple_stressors_data |>
        filter(rbd_name == rbd_full_name)

    stopifnot(nrow(multiple_stressors_data_cases) > 0)
    stopifnot(nrow(multiple_stressors_data_cases) == 192)

    # Each call to make_threshold_row now returns a full labelled row
    p <- imap(fig7_ranges, \(threshold, i) {
        start_letter <- (i - 1) * 3 + 1
        make_threshold_row(
            multiple_stressors_data_cases,
            threshold,
            start_letter
        )
    }) |>
        wrap_plots(
            ncol = 1,
            nrow = length(fig7_ranges),
            guides = "collect" # collect legends across rows
        ) +
        plot_annotation(
            title = glue(
                "Predicted Mixture Risk for {rbd_code}"
            ),
            subtitle = "All stressors, Belgium (modelled data)"
        ) +
        theme(legend.position = "bottom")

    filename <- glue(
        "images/fig7_{str_to_lower(str_replace_all(rbd_code, '_', '-'))}.png"
    )
    ggsave(
        filename = filename,
        plot = p,
        width = 30,
        height = 24,
        units = "cm",
        device = ragg::agg_png,
        res = 300
    )
    message(glue("saved {filename}"))
})
