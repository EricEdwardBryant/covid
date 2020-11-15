library(tidyverse)
library(here)
library(slider)

source(here("code/R/download-new-cases-ca.R"))
source(here("code/R/figure-new-cases.R"))

theme_set(theme_minimal())

knitr::opts_chunk$set(echo = FALSE, message = FALSE)
