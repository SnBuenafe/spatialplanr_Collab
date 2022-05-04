# Author: Tin Buenafe & Alvise Dabalà
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
# 3. x_lab: the text for the x label
# 4. y_lab: the text for the y label
# 5. title_leg: the text for the title of the legend
# 6. logarithmic: equals TRUE if you want to transform the data to log scale
# 7. central_tendency: you can decide if plot the mean or the median of the data having 
# "mean" of "median" in input ("mean" is the default option)

# Output:
# ggplot of kernel density plot which you can manipulate just as you would a ggplot object
# e.g. use of this code would be:
# fcreate_kdplot(df, palette) +
# geom_vline(xintercept = c(30), linetype = "dashed", color = "red", size = 1)

fcreate_kdplot <- function(df, palette, x_lab = NULL, y_lab = NULL, title_leg = "Legend",
                           logarithmic = FALSE, central_tendency = "mean") {
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ggridges)
  
  if(logarithmic == TRUE) {
    df <- df %>% 
      mutate(across(!rows, log))
  }
  
  x <- df %>% 
    tidyr::pivot_longer(!rows, names_to = "group", values_to = "value") %>% 
    dplyr::mutate(row_number = row_number(rows))
  
  if(central_tendency == "mean") {
    tendency_df <- x %>%
      group_by(group) %>% 
      summarise(value = mean(value))
  }
  
  if(central_tendency == "median") {
    tendency_df <- x %>% 
      group_by(group) %>% 
      summarise(value = mean(value))
  }
  
  ggRidge <- ggplot(data = x) +
    geom_density_ridges(aes(x = value, y = group, group = group, fill = group),
                        scale = 2) +
    scale_fill_manual(values = palette,
                      name = title_leg) +
    geom_vline(data = tendency_df , aes(xintercept = value, 
                                        colour = group),
               linetype = "dashed", size = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = palette,
                        name = title_leg) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_classic()
  
  return(ggRidge)
}
