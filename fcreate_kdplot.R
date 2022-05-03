# Author: Tin Buenafe
# Last updated: 05-04-22

# Purpose: To create kernel density plots of any df.

# Starting code for creating kernel density plots
# Input is the feature representation df where column names are the different "scenarios" and rows are the features. Data are the % of the features that are part of the solution

x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "scenario", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = scenario, group = scenario, fill = scenario),
                      scale = 2) +
  scale_fill_manual(values = c(`EM-Percentile-tos-126` = "#289E3D",
                               `EM-Percentile-tos-245` = "#E6C173",
                               `EM-Percentile-tos-585` = "#855600")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  theme_classic()