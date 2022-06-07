# Load packages
library(tidyverse)
# Code is adapted from: https://r-graph-gallery.com/circular-barplot.html
# df should have the following feature, value, group
# group colors
# group legends

# ----- Functions for creating circular barplot -----
# Inputs:
# 1. df: data frame should have the following column names: feature, value, group
# feature: individual bars
# value: value plotted in the y-axis
# group: grouping factors
# 2. legend_color: vector list of colors; should have the group names and their corresponding colors
# 3. legend_list: list of groups/legends of groups
fcreate_circBplot <- function(df, legend_color, legend_list) {
  
  # Adding rows to each group, creating space between the groups
  groups <- unique(df$group)
  NA_rows <- list()
  for(i in 1:length(groups)) {
    NA_rows[[i]] <- data.frame(feature = NA, value = 0, group = groups[i])
  }
  
  data <- df %>% 
    bind_rows(do.call(bind_rows, NA_rows)) %>% 
    group_by(group) %>% 
    arrange(feature)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame(matrix(NA, empty_bar*length(unique(data$group)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(as.factor(data$group)), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data)) 
  
  # Labels for each of the bars (features)
  
  # Get the name and the y position of each label
  label_data <- data
  # Calculate the angle of the labels
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar # Subtracting 0.5 so the labels are not found in the extreme left or right
  # Calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  # Flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # For the percentage lines
  grid_data <- data %>% 
    group_by(group) %>% 
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    dplyr::mutate(title = mean(c(start, end)))
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1.5
  grid_data$start <- grid_data$end - 1
  grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data, aes(x = as.factor(id), y = value, fill = group)) + 
    
    # plotting the bars
    geom_bar(aes(x = as.factor(id), y = value, fill = group), 
             stat = "identity", 
             position = 'dodge') +
    
    # defining colors of the bars
    scale_fill_manual(name = "Solution",
                      values = legend_color,
                      labels = legend_list) +
    
    # Add text showing the value of each 100/75/50/25 lines
    geom_segment(data = grid_data, 
                 aes(x = end, y = 25, xend = start, yend = 25), 
                 colour = "grey50", 
                 alpha = 1, 
                 size = 0.5 , 
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 50, xend = start, yend = 50), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 75, xend = start, yend = 75), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 100, xend = start, yend = 100), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    annotate("text", x = rep(max(data$id),4), 
             y = c(25, 50, 75, 100), 
             label = c(25, 50, 75, 100), 
             color = "grey50", 
             size=4, 
             angle = -5, 
             fontface = "bold", 
             hjust=0.5) +
    
    # setting limitations of actual plot
    ylim(-20,120) +
    theme_minimal() +
    coord_polar() + 
    
    geom_text(data = label_data, aes(x = id, y=value+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Defining colors of these lines
    scale_color_manual(name = "Features",
                       values = palette) +
    
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(0.5,4), "cm")
    )
  
  return(p)
}

# ----- Example df -----
colors <- c('grp1' = 'darkseagreen', 'grp2' = 'tan3', 
                  'grp3' = 'salmon4', 'grp4' = 'grey30')
legends <- c('1', '2', '3', '4')
df.manip <- tibble(feature = c("feat1", "feat2", "feat3", "feat4", "feat5", "feat6", "feat7", "feat8"),
                   value = seq(from = 10, to = 100, length.out = 8),
                   group = c(rep("grp1", 2), rep("grp2", 2), rep("grp3", 2), rep("grp4", 2)))
x <- fcreate_circBplot(df.manip, legend_list = legends, legend_color = colors)


