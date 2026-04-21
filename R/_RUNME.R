# Readme ----
# This dataset describes predicted chemical stressor Risk Quotients in 5 Belgian WFD river catchments across a
# 12-month period. A Bayesian Network (not included) has been used to calculate the probability
# of both individual stressors and groups (herbicide, fungicide, insecticide) and combined.
#
# NODE TYPES:
#
# RowNo: Row index. No special meaning.
# Month: Jan - Dec
# RBD: River Basin District (one of 5 Belgian catchments)
# threshold_RQ: RQ threshold used for P(#Group_XXXXX_1.Subst_YYYYYY_1.RQ_exceeds=0/1), P(#Any_RQ_exceeds=0/1), and P(#Group_XXXXX_1.Any_RQ_exceeds=0/1)
# threshold_SumRQ:RQ threshold used for P(#Group_XXXXX_1.SumRQ_exceeds=0/1)
# threshold_SumSumRQ: threshold used for P(#SumSumRQ_exceeds=0/1)
#
# P(#SumSumRQ=Z): Probability that the total summed RQ across ALL groups falls in interval Z (Z = 0..11)
# P(#SumSumRQ_exceeds=0/1): Probability that SumSumRQ does NOT (=0) or DOES (=1) exceed the threshold
# P(#Any_SumRQ_exceeds=0/1): Probability that ANY group-level SumRQ exceeds its threshold
# P(#Any_RQ_exceeds=0/1): Probability that ANY individual substance RQ exceeds its threshold
#
# P(#Group_XXXXX_1.SumRQ=Z): Probability that the sum of RQs within group XXXXX falls in interval Z
#   Groups: fungi_1 (fungicides), herbi_1 (herbicides), insec_1 (insecticides)
# P(#Group_XXXXX_1.SumRQ_exceeds=0/1): Probability that group XXXXX SumRQ does NOT/DOES exceed threshold
# P(#Group_XXXXX_1.Any_RQ_exceeds=0/1): Probability that ANY substance in group XXXXX exceeds its RQ threshold
#
# P(#Group_XXXXX_1.Subst_YYYYYY_1.RQ=Z): Probability that substance YYYYYY (within group XXXXX)
#   has an RQ falling in interval Z (Z = 0..11)
#   Substances:
#     fungi_1: chloro (chlorothalonil), pyracl (pyraclostrobin), tebuco (tebuconazole),
#              thioph (thiophanate-methyl), triflo (trifloxystrobin)
#     herbi_1: 24dich (2,4-dichlorophenoxyacetic acid), dicamb (dicamba),
#              dichlo (dichlorprop), dimete (dimethachlor), diuron, glypho (glyphosate),
#              mcpaaa (MCPA), pendim (pendimethalin)
#     insec_1: chlorp (chlorpyrifos), dimeto (dimethoate)
# P(#Group_XXXXX_1.Subst_YYYYYY_1.RQ_exceeds=0/1): Probability that substance YYYYYY RQ
#   does NOT (=0) or DOES (=1) exceed its threshold
#
# [MEAN](X): Expected value (mean) of node X
# [MEDIAN](X): Median of node X
# [MAX]P(X): The maximum probability value in the distribution of node X
# [ARGMAX]P(X): The interval/state Z at which P(X=Z) is maximised (modal state)

# Setup ----
# Install these packages if you don't have them
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtext)
  library(glue)
  library(patchwork)
  library(showtext)
  font_add_google("Sarabun", family = "Sarabun")
})

# set the latest version of the datafile here
# ! WARNING: This script is very fragile to changes in node names
# ! This is unavoidable, but expect stuff to break if they change
data_path <- "data/t.DataFile.ENCORE.2026-03-16_wResults.txt"

# Pipeline ----
## Load ----
# Load raw data file. Pivot, generate tidy columns with regex
source("R/fct_parse_nodes.R")
source("R/load_data.R")
## Transform ----
# Replace terse computer names with pretty human names
source("R/format_data.R")
# Merge from 12 intervals to 6 (inc 0-0), and sum probabilities
source("R/merge_intervals.R")

## Plot ----
# Set colour scheme, axis, and fonts
source("R/themes.R")

# some more defaults
geom_col_width <- 0.9 # 0 (none) to 1 (full)

# Generate Figure(s) 1: P(#Group_XXXXX_1.Subst_YYYYYY_1.RQ=Z) (x) for each month (y), stressor (facet), and RBD (plot); RQ interval (fill)
source("R/make_fig1.R")
# Generate Figure 2: P(#Group_XXXXX_1.SumRQ=Z) (x) for each month (y), group (facet-x), and RBD (facet-y); RQ interval (fill)
source("R/make_fig2.R")
# Generate Figure 3: For RBD="MAAS VL" Group="All", compare P(#SumSumRQ=Z) to P(#Any_SumRQ_exceeds=0/1) and P(#Any_RQ_exceeds=0/1) (facet-x) at threshold_SumRQ and threshold_SumSumRQ %in% c(0.1, 1); RQ interval (fill), month (x), P(of group in interval, or threshold exceeded) (y). Bars use fill for intervals, line colour for thresholds
source("R/make_fig3.R")
