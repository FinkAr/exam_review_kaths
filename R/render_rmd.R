# load required packages
if (!"pacman" %in% installed.packages()) { install.packages("pacman") }
pacman::p_load(
  knitr, rmarkdown, 
  tidyverse, here 
)

source(here("R/functions.R"))

# filepaths of the data from each participant:
datafiles_participants <- list.files(
  here::here("data/results"), 
  full.names = FALSE
  ) %>% 
  as_tibble() %>% 
  rename(filepath = value) %>% 
  filter(!str_detect(filepath, "_results")) %>% 
  pull()

 

render_review(filename_of_data = datafiles_participants[1])

# build pdf for all participants:
walk(datafiles_participants, render_review)


