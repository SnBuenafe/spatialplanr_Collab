# Author: Tin Buenafe
# Last Updated: 05-05-22

# Purpose: Create selection frequency plots for a list of solutions
# Input:
# 1. sol : list of solutions
# 2. names : desired column names of each of the solutions in the output object
# 3. PlanUnits : sf of the planning units
# Output:
# list of two items
# $low_regret : sf object, with each of the solutions as columns, selection as the # of times the planning unit was selected
# $plot : selection frequency plot

fcreate_selfreq <- function(sol, names, PlanUnits) {
  
  library(sf)
  library(tidyverse)
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  
  PlanUnits %<>% dplyr::mutate(cellID = row_number())
  
  df <- list() # empty list
  for(i in 1:length(names)) {
    df[[i]] <- sol[[i]] %>% dplyr::select(solution_1) %>% 
      dplyr::rename(!!sym(names[i]) := solution_1) %>% 
      as_tibble()
  }
  
  tmp <- df[[1]]
  for (i in 2:length(names)) {
    tmp <- tmp %>% 
      left_join(df[[i]], .)
  }
  
  # Create low-regret sf object
  low_regret <- tmp %>% 
    dplyr::select(-geometry) %>% 
    mutate(selection = rowSums(., na.rm = TRUE)) %>% 
    dplyr::mutate(cellID = row_number()) %>% 
    full_join(., PlanUnits, by = "cellID") %>% 
    st_as_sf(sf_column_name = "geometry")
  
  length = length(names)
  
  ggSelFreq <- ggplot() +
    geom_sf(data = low_regret, aes(fill = as.factor(selection)), color = NA, size = 0.01) +
    coord_sf(xlim = st_bbox(low_regret)$xlim, ylim = st_bbox(low_regret)$ylim) +
    scale_fill_brewer(name = "Selection Frequency",
                      palette = "PuBu", aesthetics = "fill") +
    theme_bw()
  
  object <- list(low_regret = low_regret, plot = ggSelFreq)
  
  return(object)
}