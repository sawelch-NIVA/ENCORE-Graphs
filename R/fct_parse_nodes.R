# Parse metric ----
parse_metric_name <- function(metric) {
  # Strip leading "P.." prefix
  stripped <- str_remove(metric, "^P\\.\\.")

  # -- Determine group and substance --
  has_group <- str_detect(stripped, "^Group_")
  has_subst <- str_detect(stripped, "Subst_")

  group_code <- case_when(
    has_group ~ str_extract(stripped, "^Group_([a-z]+_[0-9]+)", group = 1),
    TRUE ~ NA_character_
  )

  subst_code <- case_when(
    has_subst ~ str_extract(stripped, "Subst_([a-z0-9]+)_[0-9]+", group = 1),
    TRUE ~ NA_character_
  )

  # -- Determine the RQ operation --
  # Order matters: SumSumRQ before SumRQ
  rq_op <- case_when(
    str_detect(stripped, "SumSumRQ") ~ "SumSumRQ",
    str_detect(stripped, "SumRQ") ~ "SumRQ",
    str_detect(stripped, "Any_RQ") ~ "Any_RQ",
    str_detect(stripped, "\\.RQ[_\\.]") ~ "RQ", # substance-level individual RQ
    TRUE ~ NA_character_
  )

  # -- Determine comparison type and value --
  is_exceeds <- str_detect(stripped, "exceeds")

  exceeds_val <- case_when(
    str_detect(stripped, "exceeds\\.1\\.") ~ TRUE,
    str_detect(stripped, "exceeds\\.0\\.") ~ FALSE,
    is_exceeds ~ NA # exceeds present but no 0/1: fail below
  )

  interval_val <- case_when(
    !is_exceeds ~ str_extract(stripped, "\\.([0-9]{1,2})\\.$", group = 1) |>
      as.integer(),
    TRUE ~ NA_integer_
  )

  list(
    group_code = group_code,
    subst_code = subst_code,
    rq_op = rq_op,
    is_exceeds = is_exceeds,
    exceeds_val = exceeds_val,
    interval_val = interval_val
  )
}

# Validate parsed metrics ----
validate_parsed_metrics <- function(parsed_df) {
  # Known valid values
  valid_groups <- c("fungi_1", "herbi_1", "insec_1")
  valid_substs <- c(
    "chloro",
    "pyracl",
    "tebuco",
    "thioph",
    "triflo", # fungi_1
    "24dich",
    "dicamb",
    "dichlo",
    "dimete",
    "diuron", # herbi_1
    "glypho",
    "mcpaaa",
    "pendim", # herbi_1 cont.
    "chlorp",
    "dimeto" # insec_1
  )
  valid_rq_ops <- c("SumSumRQ", "SumRQ", "Any_RQ", "RQ")
  valid_intervals <- 0:11

  problems <- character()

  # group_code: must be NA or in valid set
  bad_groups <- parsed_df |>
    filter(!is.na(group_code), !group_code %in% valid_groups) |>
    pull(metric)
  if (length(bad_groups)) {
    problems <- c(
      problems,
      glue::glue(
        "Unexpected group codes in: {paste(bad_groups, collapse = ', ')}"
      )
    )
  }

  # subst_code: must be NA or in valid set
  bad_substs <- parsed_df |>
    filter(!is.na(subst_code), !subst_code %in% valid_substs) |>
    pull(metric)
  if (length(bad_substs)) {
    problems <- c(
      problems,
      glue::glue(
        "Unexpected substance codes in: {paste(bad_substs, collapse = ', ')}"
      )
    )
  }

  # rq_op: must not be NA
  missing_rq_op <- parsed_df |>
    filter(is.na(rq_op)) |>
    pull(metric)
  if (length(missing_rq_op)) {
    problems <- c(
      problems,
      glue::glue(
        "Could not parse rq_op for: {paste(missing_rq_op, collapse = ', ')}"
      )
    )
  }

  # exceeds rows must have a resolved TRUE/FALSE
  bad_exceeds <- parsed_df |>
    filter(is_exceeds, is.na(exceeds_val)) |>
    pull(metric)
  if (length(bad_exceeds)) {
    problems <- c(
      problems,
      glue::glue(
        "Exceeds rows with no 0/1 resolved: {paste(bad_exceeds, collapse = ', ')}"
      )
    )
  }

  # interval rows must have a value in 0:11
  bad_intervals <- parsed_df |>
    filter(!is_exceeds, !interval_val %in% valid_intervals) |>
    pull(metric)
  if (length(bad_intervals)) {
    problems <- c(
      problems,
      glue::glue(
        "Interval values out of range 0-11: {paste(bad_intervals, collapse = ', ')}"
      )
    )
  }

  # substance rows must have a group
  subst_without_group <- parsed_df |>
    filter(!is.na(subst_code), is.na(group_code)) |>
    pull(metric)
  if (length(subst_without_group)) {
    problems <- c(
      problems,
      glue::glue(
        "Substance rows missing group: {paste(subst_without_group, collapse = ', ')}"
      )
    )
  }

  if (length(problems)) {
    stop(
      "Metric parsing validation failed:\n",
      paste0("  - ", problems, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(parsed_df)
}
