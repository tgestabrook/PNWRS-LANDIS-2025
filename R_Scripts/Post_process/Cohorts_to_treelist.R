library(terra)
library(tidyverse)
library(tidyterra)

Input_treelist.df <- read.csv(file.path(wdir,paste0(LANDIS.EXTENT,"_tree_list.csv")))

unique_cohort.df <- Input_treelist.df |>
  dplyr::select(tm_id, SpecCode, AGE_limit_by_longevity, AG_biomass_gm2) |>
  group_by(tm_id, SpecCode, AGE_limit_by_longevity) |>
  summarise(sum(AG_biomass_gm2)) |>
  na.omit()

LANDIS_cohorts <- read.csv()












