suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtext)
  library(glue)
})

# set the latest version of the datafile here
data_path <- "data/t.DataFile.ENCORE.2026-03-16_wResults.txt"

# Load raw data file. Pivot, generate tidy columns with regex
source("R/load_data.R")
# Replace terse computer names with pretty human names
source("R/format_data.R")
# Merge from 12 intervals to 6 (inc 0-0), and sum probabilities
source("R/merge_intervals.R")
# Set colour scheme, axis, and fonts
source("R/themes.R")
source("R/make_fig1.R")
source("R/make_fig2.R")
source("R/make_fig3.R")
# Figure 3
# TODO: Add letter tags per facet
# TODO: Use BEEMAAS_VL as example, make 3 (group RQ metric) x 2 (>0.1 and 1) grid
# TODO: Use colour instead of fill for CA + IA and IA bars <-
