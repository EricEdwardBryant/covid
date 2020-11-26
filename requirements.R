library(tidyverse)
library(patchwork)
library(here)
library(slider)

source(here("code/R/download-data-ca.R"))
source(here("code/R/download-new-cases-ca.R"))
source(here("code/R/download-hospitalization-ca.R"))
source(here("code/R/figure-new-cases.R"))
source(here("code/R/figure-daily-change.R"))
source(here("code/R/figure-hospitalizations.R"))

theme_set(
  theme_minimal() +
    theme(panel.grid.minor.y = element_blank())
)

knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  fig.width = 3.5,
  fig.align = "center",
  dpi = 300,
  out.width = "350px"
)

options(
  gtsummary.as_gt.addl_cmds =
    "gt::tab_options(table.font.size = 'small', data_row.padding = gt::px(1))"
)
