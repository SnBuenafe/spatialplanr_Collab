# Author: Tin Buenafe
# Last Updated: 05-05-22

# Create low regret areas for specific approach
create_LowRegretSf <- function(solution_list, col_names, PUs, scenario = FALSE) {
  
  df <- list() # empty list
  for (i in 1:length(col_names)) {
    df[[i]] <- solution_list[[i]] %>% dplyr::select(solution_1) %>% 
      rename(!!sym(col_names[i]) := solution_1) %>% 
      as_tibble()
  }
  
  tmp <- df[[1]]
  for (i in 2:length(col_names)) {
    tmp <- tmp %>% 
      left_join(df[[i]], .)
  }
  
  tmp %<>% dplyr::select(-geometry) %>% 
    mutate(selection = rowSums(., na.rm = TRUE)) %>% 
    dplyr::mutate(cellID = row_number())
  
  PUs_temp <- PUs %>% 
    dplyr::mutate(cellID = row_number())
  
  # Create the low-regret sf object
  low_regret <- full_join(tmp, PUs_temp, by = "cellID") %>% 
    st_as_sf(sf_column_name = "geometry") %>% 
    left_join(., UniformCost %>% as_tibble(), by = "geometry") %>% 
    st_as_sf(sf_column_name = "geometry") 
  
  if (isTRUE(scenario)) {
    low_regret %<>% 
      dplyr::mutate(solution_1 = ifelse(selection == 3, 1, 0))
  } else {
    low_regret %<>% 
      dplyr::mutate(solution_1 = ifelse(selection == 4, 1, 0))    
  }
  
  return(low_regret)
}

plot_SelectionFrequency <- function(data, land) {
  gg <- ggplot() + geom_sf(data = data, aes(fill = as.factor(selection)), color = NA, size = 0.01) +
    geom_sf(data = land, color = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(data)$xlim, ylim = st_bbox(data)$ylim) +
    scale_fill_brewer(name = "Selection Frequency",
                      palette = "PuBu", aesthetics = "fill") +
    theme_bw() +
    labs(subtitle = "Variability in GCMs")
}

sFreq <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(sFreq, paste0(output_lowregret, "sFreq3-EM-Percentile-585.rds")) # save low-regret solution
(ggFreq <- plot_SelectionFrequency(sFreq, land) + ggtitle("Metric Theme", subtitle = "Percentile (SSP 5-8.5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "Freq-Percentile-Ensemble-tos-585.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

