suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtext)
  library(glue)
})

source("R/load_data.R")
# Data
# TODO: Update for new 0 - 0 interval
# TODO: Fix whatever's going on with Probability_perc not being a percentage
source("R/format_data.R")
source("R/themes.R")
# TODO: Add white colour for 0 - 0 interval
source("R/make_fig1.R")
source("R/make_fig2.R")
source("R/make_fig3.R")
# Figure 3
# TODO: Add letter tags per facet
# TODO: Use BEEMAAS_VL as example, make 3 (group RQ metric) x 2 (>0.1 and 1) grid
# TODO: Use colour instead of fill for CA + IA and IA bars
