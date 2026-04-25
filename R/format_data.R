stressor_group_names <- tribble(
  ~stressor_group , ~stressor_group_name ,
  "fungi"         , "Fungicides"         ,
  "herbi"         , "Herbicides"         ,
  "insec"         , "Insecticides"       ,
  "all"           , "All stressors"
)
stressor_group_icons <- tribble(
  ~stressor_group , ~icon_path           ,
  "fungi"         , "icons/mushroom.png" ,
  "herbi"         , "icons/leaf.png"     ,
  "insec"         , "icons/beetle.png"
)
rbd_names <- tribble(
  ~RBD           , ~rbd_name ,
  "BEESCAUT_RW"  , "ESCAUT"  ,
  "BEMAAS_VL"    , "MAAS"    ,
  "BEMEUSE_RW"   , "MEUSE"   ,
  "BERHIN_RW"    , "RHIN"    ,
  "BESCHELDE_VL" , "SCHELDE"
) |>
  mutate(
    rbd_name = factor(
      rbd_name,
      levels = c("ESCAUT", "SCHELDE", "MEUSE", "MAAS", "RHIN"),
      ordered = TRUE
    )
  ) |>
  arrange(rbd_name)


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
  left_join(stressor_group_names, by = join_by(stressor_group), keep = FALSE) |>
  left_join(stressor_group_icons, by = join_by(stressor_group), keep = FALSE) |>
  distinct() |>
  arrange(stressor_group_name, stressor_name) |>
  mutate(label_letter = letters[row_number()])

rq_level_ranges <- tribble(
  ~RQ_level , ~RQ_range        , ~RQ_lower_bound ,
          0 , "0 - 0"          , NA_integer_     ,
          1 , "0 - 0.001"      ,  0              ,
          2 , "0.001 - 0.0032" ,  0.001          ,
          3 , "0.0032 - 0.01"  ,  0.0032         ,
          4 , "0.01 - 0.032"   ,  0.01           ,
          5 , "0.032 - 0.1"    ,  0.032          ,
          6 , "0.1 - 0.32"     ,  0.1            ,
          7 , "0.32 - 1"       ,  0.32           ,
          8 , "1 - 3.2"        ,  1              ,
          9 , "3.2 - 10"       ,  3.2            ,
         10 , "10 - 32"        , 10              ,
         11 , "32 - Inf"       , 32
)

data_long_pretty <- data_long |>
  mutate(
    Month_abb = factor(Month, levels = 1:12, labels = month.abb),
    Probability_perc = value * 100
  ) |>
  left_join(rbd_names, by = join_by(RBD)) |>
  left_join(
    stressor_names |>
      select(stressor_code, stressor_name, label_letter, icon_path),
    by = join_by(stressor_code)
  ) |>
  arrange(label_letter) |>
  left_join(
    stressor_group_names,
    by = join_by(stressor_group),
    unmatched = "error"
  ) |>
  left_join(rq_level_ranges, by = join_by(RQ_level)) |>
  mutate(
    stressor_name_group_md = glue(
      "{label_letter}) {stressor_name} <img src='{icon_path}' width='10' vertical-align='bottom'/>"
    )
  ) |>
  mutate(
    # threshold equality already verified in load_data.R
    sum_operation_threshold = if_else(
      exceedence_boolean,
      as.double(threshold_merged),
      NA_real_
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
    metric,
    exceedence_boolean,
    Probability_perc
  ) |>
  ungroup() |>
  distinct()

# check we haven't added (or removed) any rows in the process
stopifnot(nrow(data_long) == nrow(data_long_pretty))
