# load required packages
if (!"pacman" %in% installed.packages()) { install.packages("pacman") }
pacman::p_load(
  knitr, rmarkdown, 
  tidyverse, here, fs,
  embedr, kableExtra,
  glue, mirt
)

source(here("R/functions.R"))

# filepaths of the data from each participant:
datafiles_participants <- dir_map(
  path = here("data/results"),
  fun = basename
  ) %>% 
  flatten_chr() %>% 
  str_subset(., "_WS")

# build pdf for all participants:
walk(datafiles_participants, render_review)

# render_review(datafiles_participants[1])
library("psycModel")

html_to_pdf(dir = "out", render_exist = T)
