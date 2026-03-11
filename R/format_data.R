rbd_names <- tribble(
  ~RBD           , ~rbd_name                  ,
  "BEESCAUT_RW"  , "Scheldt Basin (Wallonia)" ,
  "BEMAAS_VL"    , "Meuse Basin (Flanders)"   ,
  "BEMEUSE_RW"   , "Meuse Basin (Wallonia)"   ,
  "BERHIN_RW"    , "Rhine Basin (Wallonia)"   ,
  "BESCHELDE_VL" , "Scheldt Basin (Flanders)"
)

stressor_group_names <- tribble(
  ~stressor_group , ~stressor_group_name ,
  "fungi"         , "Fungicide"          ,
  "herbi"         , "Herbicide"          ,
  "insec"         , "Insecticide"
)

h <- tribble(
  ~RBD           , ~rbd_name                  ,
  "BEESCAUT_RW"  , "Scheldt Basin (Wallonia)" ,
  "BEMAAS_VL"    , "Meuse Basin (Flanders)"   ,
  "BEMEUSE_RW"   , "Meuse Basin (Wallonia)"   ,
  "BERHIN_RW"    , "Rhine Basin (Wallonia)"   ,
  "BESCHELDE_VL" , "Scheldt Basin (Flanders)"
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
  left_join(data_long |> select(stressor_code, stressor_group)) |>
  left_join(stressor_group_names, by = join_by(stressor_group)) |>
  distinct() |>
  arrange(stressor_group_name, stressor_name) |>
  mutate(label_letter = letters[row_number()])

rq_level_ranges <- tribble(
  ~RQ_level , ~RQ_range    ,
          0 , "0 - 0.01"   ,
          1 , "0.01 - 0.1" ,
          2 , "0.1 - 1"    ,
          3 , "1 - 10"     ,
          4 , "10 - Inf"
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
  left_join(stressor_group_names, by = join_by(stressor_group)) |>
  left_join(rq_level_ranges, by = join_by(RQ_level)) |>
  mutate(
    stressor_name_group_md = glue(
      "**{label_letter})** {stressor_name} ({stressor_group_name})"
    )
  ) |>
  mutate(
    stressor_name_group_md = factor(
      stressor_name_group_md,
      levels = unique(stressor_name_group_md)
    )
  ) |>
  select(
    Month_abb,
    RBD,
    rbd_name,
    stressor_group,
    stressor_group_name,
    stressor_name_group_md,
    stressor_code,
    stressor_name,
    stressor_type,
    RQ_level,
    RQ_range,
    RQ_operation,
    value,
    exceedence_boolean,
    Probability_perc
  )

data_long_pretty
