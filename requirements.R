library(tidyverse)
library(here)
library(slider)

source(here("code/R/download-new-cases-ca.R"))
source(here("code/R/figure-new-cases.R"))

theme_set(
  theme_minimal() +
    theme(panel.grid.minor.y = element_blank())
)

knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  fig.width = 3.5,
  fig.height = 3,
  dpi = 300,
  out.width = "300px"
)
