data <- read.delim(
  data_path,
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

data <- data |> as_tibble()

# Warn if unexpected shape
if (is.null(data) | nrow(data) != 300 | ncol(data) != 358) {
  warning(
    "Imported data dimensions different to expected values. Stuff may break."
  )
}

# check the threshold columns are always identical
inequal_thresholds <- data |>
  reframe(
    thresholds_identical = all(
      threshold_RQ == threshold_SumRQ,
      threshold_RQ == threshold_SumSumRQ
    )
  ) |>
  pull(thresholds_identical)

if (!is.null(inequal_thresholds) & !inequal_thresholds) {
  warning(
    "Code assumes threshold_ columns are always identical. Stuff may break."
  )
}

# Build long data ----

# Pivot first
data_long_raw <- data |>
  mutate(threshold_merged = threshold_RQ) |>
  select(Month, RBD, starts_with("P.."), threshold_merged) |>
  pivot_longer(
    cols = starts_with("P.."),
    names_to = "metric",
    values_to = "value"
  )

# at this point, any non-threshold (so interval) row shows up 5x as much as it should, because there are 5 threshold states

# Parse metric names -> one row per metric (distinct metrics only, then join)
metric_parsed <- data_long_raw |>
  distinct(metric) |> # only do the operation once per metric or it's slow as all hell
  mutate(parsed = map(metric, parse_metric_name)) |>
  unnest_wider(parsed) |>
  validate_parsed_metrics() # fails loud here if anything unexpected


# then join it to the real dataset (keep the join strict for safety)
data_long <- data_long_raw |>
  left_join(metric_parsed, unmatched = "error", by = join_by(metric)) |>
  # For interval rows, threshold_merged varies across the 5 raw threshold-level
  # copies but the probability value should be (near-)identical. Collapse
  # explicitly by averaging value across copies rather than relying on
  # full-row distinct(), which would keep any rows that differ by float noise.
  mutate(
    threshold_merged = case_when(is_exceeds ~ threshold_merged, .default = NA)
  ) |>
  reframe(
    value = mean(value),
    .by = c(
      metric,
      Month,
      RBD,
      group_code,
      subst_code,
      rq_op,
      is_exceeds,
      exceeds_val,
      interval_val,
      threshold_merged
    )
  )

# check we don't have any weird groups
# if we've done our job, none of these groups summed probabilities should be > 1 +/- some floating point weirdness
summed_probs_gt_1 <- data_long |>
  reframe(
    .by = c(
      "Month",
      "RBD",
      "threshold_merged",
      "rq_op",
      "group_code",
      "subst_code"
    ),
    n = n(),
    value_sum = sum(value)
  ) |>
  filter(value_sum > 1.05) # close enough

# if this triggers your grouping variables are messed up :(
stopifnot(nrow(summed_probs_gt_1) == 0)

# Join parsed components back
data_long <- data_long |>
  select(
    metric,
    Month,
    RBD,
    group_code,
    subst_code,
    rq_op,
    is_exceeds,
    exceeds_val,
    interval_val,
    value,
    threshold_merged
  )

# Validation checks
# Just check all scenario columns (stressor/month/rbd exist)
stopifnot(!all(is.na(data_long$value)))
stopifnot(nrow(data_long) > 0)
stopifnot(all(!is.na(data_long$Month)))
stopifnot(all(!is.na(data_long$RBD)))
# RQ rows (substance-level) must have both a group and a substance code
stopifnot(
  nrow(filter(
    data_long,
    rq_op == "RQ",
    is.na(group_code) | is.na(subst_code)
  )) ==
    0
)

# Exceeds rows must have a resolved TRUE/FALSE value (not NA)
stopifnot(
  nrow(filter(data_long, is_exceeds, is.na(exceeds_val))) == 0
)

# Non-exceeds (interval) rows must have an interval value
stopifnot(
  nrow(filter(data_long, !is_exceeds, is.na(interval_val))) == 0
)

# For each node (Month × RBD × group × substance × rq_op), interval
# probabilities must sum to 1 (within floating-point tolerance)
bad_sums <- data_long |>
  filter(!is_exceeds) |>
  reframe(
    p_summed = sum(value),
    group_n = n(),
    .by = c(Month, RBD, group_code, subst_code, rq_op),
    all_metrics = paste(metric)
  ) |>
  filter(abs(p_summed - 1) > 0.05)

if (nrow(bad_sums) > 0) {
  stop(
    nrow(bad_sums),
    " node-month-RBD combination(s) have interval probabilities not summing to 1.",
    call. = FALSE
  )
}

# Rename and transform columns ----
# Drop P(exceeds = 0) rows — P(exceeds = 1) is all that is needed downstream,
# and keeping both would cause them to be summed together in merge_intervals.R
data_long <- data_long |>
  filter_out(is_exceeds, !exceeds_val)

data_long <- data_long |>
  mutate(
    stressor_group = if_else(
      !is.na(group_code),
      str_remove(group_code, "_[0-9]+$"),
      "all"
    ),
    stressor_code = subst_code,
    sum_operation = rq_op,
    exceedence_boolean = is_exceeds,
    comparison_operation = if_else(is_exceeds, "exceeds", "interval"),
    RQ_level = interval_val
  ) |>
  select(
    metric,
    Month,
    RBD,
    stressor_group,
    stressor_code,
    sum_operation,
    exceedence_boolean,
    comparison_operation,
    RQ_level,
    value,
    threshold_merged
  )

# quick data report
stressors <- data_long |> pull(stressor_code) |> unique()
months <- data_long |> pull(Month) |> unique()
RBD <- data_long |> pull(RBD) |> unique()
stressor_group <- data_long |> pull(stressor_group) |> unique()
sum_operation <- data_long |> pull(sum_operation) |> unique()
message(glue(
  "-- Data Loaded --
  Stressors ({length(stressors)}): {toString(stressors)}
  Months: ({length(months)}): {toString(months)}
  RBDs: ({length(RBD)}): {toString(RBD)}
  Stressor Groups: ({length(stressor_group)}): {toString(stressor_group)}
  Sum Operations: ({length(sum_operation)}): {toString(sum_operation)}"
))
