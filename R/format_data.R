stressor_group_names <- tribble(
  ~stressor_group , ~stressor_group_name ,
  "fungi"         , "Fungicide"          ,
  "herbi"         , "Herbicide"          ,
  "insec"         , "Insecticide"
)

rbd_names <- tribble(
  ~RBD           , ~rbd_name                 ,
  "BEESCAUT_RW"  , "Scheldt Basin, Wallonia" ,
  "BEMAAS_VL"    , "Meuse Basin, Flanders"   ,
  "BEMEUSE_RW"   , "Meuse Basin, Wallonia"   ,
  "BERHIN_RW"    , "Rhine Basin, Wallonia"   ,
  "BESCHELDE_VL" , "Scheldt Basin, Flanders"
)


stressor_names <- tribble(
  ~stressor_code , ~stressor_name       ,
  "chloro"       , "Chlorothalonil"     ,
  "pyracl"       , "Pyraclostrobin"     ,
  "tebuco"       , "Tebuconazole"       ,
  "thioph"       , "Thiophanate-methyl" ,
  "triflo"       , "Trifloxystrobin"    ,
  "24dich"       , "2,4-D"              ,
  "dicamb"       , "Dicamba"            ,
  "dichlo"       , "Dichlorprop"        ,
  "dimete"       , "Dimethenamid(-p)"   ,
  "diuron"       , "Diuron"             ,
  "glypho"       , "Glyphosate"         ,
  "mcpaaa"       , "MCPA"               ,
  "pendim"       , "Pendimethalin"      ,
  "chlorp"       , "Chlorpyrifos"       ,
  "dimeto"       , "Dimethoate"
)

stressor_names <- stressor_names |>
  left_join(
    data_long |> select(stressor_code, stressor_group),
    by = join_by(stressor_code)
  ) |>
  left_join(stressor_group_names, by = join_by(stressor_group)) |>
  distinct() |>
  arrange(stressor_group_name, stressor_name) |>
  mutate(label_letter = letters[row_number()])

rq_level_ranges <- tribble(
  ~RQ_level , ~RQ_range    , ~RQ_lower_bound ,
          0 , "0 - 0.01"   ,  0              ,
          1 , "0.01 - 0.1" ,  0.01           ,
          2 , "0.1 - 1"    ,  0.1            ,
          3 , "1 - 10"     ,  1              ,
          4 , "10 - Inf"   , 10
)


data_long_pretty <- data_long |>
  mutate(
    Month_abb = factor(Month, levels = 1:12, labels = month.abb),
    Probability_perc = value * 100
  ) |>
  left_join(rbd_names, by = join_by(RBD)) |>
  left_join(
    stressor_names |> select(stressor_code, stressor_name, label_letter),
    by = join_by(stressor_code)
  ) |>
  arrange(label_letter) |>
  left_join(stressor_group_names, by = join_by(stressor_group)) |>
  left_join(rq_level_ranges, by = join_by(RQ_level)) |>
  mutate(
    stressor_name_group_md = factor(glue(
      "{label_letter}) **{stressor_name}** ({stressor_group_name})"
    ))
  ) |>
  mutate(
    sum_operation_threshold = case_when(
      # when all thresholds are the same and it's a threshold metric, not a range metric, use the threshold value
      (threshold_RQ == threshold_SumRQ &
        threshold_SumRQ == threshold_SumSumRQ) &
        exceedence_boolean ~ as.double(threshold_RQ),
      # if it's a range metric, leave as NA
      !exceedence_boolean ~ NA_real_,
      # Otherwise, something's gone wrong
      TRUE ~ NA_real_
    )
  ) |>
  filter_out(is.na(sum_operation) & !is.na(sum_operation_threshold)) |> # because of the sum thresholds work, we get duplication of rows
  select(
    Month_abb,
    RBD,
    rbd_name,
    stressor_group,
    stressor_group_name,
    stressor_name_group_md,
    stressor_code,
    stressor_name,
    sum_operation,
    sum_operation_threshold,
    RQ_level,
    RQ_range,
    comparison_operation,
    value,
    exceedence_boolean,
    Probability_perc
  ) |>
  ungroup() |>
  distinct()
