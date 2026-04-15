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
# Build long data ----
{
  # Pivot first
  data_long_raw <- data |>
    select(Month, RBD, starts_with("P.."), starts_with("threshold")) |>
    pivot_longer(
      cols = starts_with("P.."),
      names_to = "metric",
      values_to = "value"
    )

  # Parse metric names -> one row per metric (distinct metrics only, then join)
  metric_parsed <- data_long_raw |>
    distinct(metric) |>
    mutate(parsed = map(metric, parse_metric_name)) |>
    unnest_wider(parsed) |>
    validate_parsed_metrics() # fails loud here if anything unexpected

  # Join parsed components back
  data_long <- data_long_raw |>
    left_join(metric_parsed, by = "metric") |>
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
      threshold_RQ,
      threshold_SumRQ,
      threshold_SumSumRQ
    )
}

# Validation checks
# Just check if any column is entirely empty/invalid
stopifnot(!all(is.na(data_long$value)))
stopifnot(!all(is.na(data_long$RQ_level)))
stopifnot(!all(is.na(data_long$stressor_group)))
stopifnot(all(data_long$comparison_operation %in% c("exceeds", "interval")))
stopifnot(all(data_long$exceedence_boolean %in% c(TRUE, FALSE)))
stopifnot(nrow(data_long) > 0)
stopifnot(all(!is.na(data_long$Month)))
stopifnot(all(!is.na(data_long$RBD)))
