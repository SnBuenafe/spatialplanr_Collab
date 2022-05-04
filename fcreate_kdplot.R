# Author: Tin Buenafe
# Last updated: 05-04-22

# Purpose: To create kernel density plots of any df.
# Input: 
# 1. df: data frame with the following column names
# - rows (could be features, planning units, etc.)
# - other column names represent the different groups (e.g. different solutions)
# the values represent the values that we want frequency distributions of (e.g. % of features, ecosystem benefit)
# - values under the rows column indicate the feature name or planning unit number
# 2. palette: group names with the colors as a vector list
# e.g. palette <- c(`EM-Percentile-tos-126` = "#289E3D", `EM-Percentile-tos-245` = "#E6C173", `EM-Percentile-tos-585` = "#855600")

# Output:
# ggplot of kernel density plot which you can manipulate just as you would a ggplot object
# e.g. use of this code would be:
# fcreate_kdplot(df, palette) +
# geom_vline(xintercept = c(30), linetype = "dashed", color = "red", size = 1)

fcreate_kdplot <- function(df, palette) {
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ggridges)
  
  x <- df %>% 
    tidyr::pivot_longer(!rows, names_to = "group", values_to = "value") %>% 
    dplyr::mutate(row_number = row_number(rows))
  
  ggRidge <- ggplot(data = x) +
    geom_density_ridges(aes(x = value, y = group, group = group, fill = group),
                        scale = 2) +
    scale_fill_manual(values = palette) +
    theme_classic()
  
  return(ggRidge)
}
