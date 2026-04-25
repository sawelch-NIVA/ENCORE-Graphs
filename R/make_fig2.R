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
        group_and_n = glue(
            "<img src='{icon_path}' width='10' vertical-align='bottom'/> {stressor_group_name} (n = {stressor_sample_size})"
        ),
        rbd_and_group_md = glue(
            "{facet_letter}) **{rbd_name}** ({stressor_group_name} (*n = {stressor_sample_size}*))"
        )
    ) |>
    mutate(
        group_and_n = factor(
            group_and_n,
            levels = unique(group_and_n[order(stressor_group_name)])
        ),
    )

# Build letter-label dataframe matching facet_grid row x col combinations
# dir = "h" means letters go left-to-right across columns, then down rows
# which matches facet_grid(rows = rbd_name, cols = group_and_n)
facet_labels <- grouped_stressors_data |>
    distinct(rbd_name, group_and_n, stressor_group) |>
    arrange(rbd_name, stressor_group) |> # match facet_grid ordering
    ungroup() |>
    mutate(facet_letter = glue("{letters[row_number()]})"))

stopifnot(nrow(grouped_stressors_data) > 0)

p_fig2 <- grouped_stressors_data |>
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
        rows = vars(fct_inorder(rbd_name)),
        cols = vars(group_and_n),
        axes = "all_x",
        switch = "y"
    ) +
    scale_x_continuous_probability(limits = NULL) +
    scale_y_discrete_months() +
    labs(
        x = "Probability that RQ in Interval",
        y = NULL,
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
        strip.text.y.left = element_markdown(size = 11),
        legend.position = "bottom",
        panel.spacing = unit(1.1, "lines")
    ) +
    geom_text(
        data = facet_labels,
        aes(label = facet_letter),
        x = -0.070,
        y = 13.5,
        inherit.aes = FALSE,
        check_overlap = FALSE,
        family = "Sarabun",
        fontface = "bold",
        size.unit = "pt",
        size = 13
    ) +
    coord_cartesian(clip = "off")

label_grob <- textGrob(
    "River Basin District",
    rot = 90,
    gp = gpar(fontfamily = "Sarabun", fontsize = 13, fontface = "bold")
)

p_annotated <- wrap_plots(
    list(wrap_elements(full = label_grob), p_fig2),
    widths = c(1, 19)
)

filename <- "images/fig2_grouped_stressors.png"
ggsave(
    filename = filename,
    plot = p_annotated,
    width = 24,
    height = 30,
    units = "cm",
    device = ragg::agg_png,
    res = 300
)
message(paste0("saved ", filename))
