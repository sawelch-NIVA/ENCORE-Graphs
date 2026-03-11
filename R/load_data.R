library(tidyverse)

data <- read.delim(
  "data/t.DataFile.ENCORE.2026-03-09_wResults.txt",
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE
)

data <- data |> as_tibble()

data_long <- data |>
  select(Month, RBD, starts_with("P..")) |>
  pivot_longer(
    cols = starts_with("P.."),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    stressor_group = str_extract(
      metric,
      "Group_([a-z]{5})|Subst_",
      group = 1
    ) |>
      coalesce("all"),
    stressor_code = case_when(
      str_detect(metric, "Subst_") ~ str_extract(
        metric,
        "Subst_([a-z0-9]+)_",
        group = 1
      ),
      TRUE ~ NA_character_
    ),
    stressor_type = case_when(
      str_detect(metric, "SumSumRQ") ~ "SumSumRQ",
      str_detect(metric, "SumRQ") ~ "SumRQ",
      str_detect(metric, "Any_RQ") ~ "Any_RQ",
      TRUE ~ NA_character_
    ),
    RQ_operation = case_when(
      str_detect(metric, "exceeds") ~ "exceeds",
      str_detect(metric, "level") ~ "level",
      TRUE ~ NA_character_
    ),
    exceedence_boolean = case_when(
      str_detect(metric, "exceeds.1.") ~ TRUE,
      str_detect(metric, "exceeds.0.") ~ FALSE,
      TRUE ~ FALSE,
    ),
    RQ_level = case_when(
      str_detect(metric, "level") ~ str_extract(
        metric,
        "level\\.([0-4])\\.",
        group = 1
      ) |>
        as.integer(),
      TRUE ~ NA_integer_
    )
  ) |>
  select(
    Month,
    RBD,
    stressor_group,
    stressor_code,
    stressor_type,
    RQ_level,
    RQ_operation,
    exceedence_boolean,
    value
  ) |>
  distinct()

data_long
