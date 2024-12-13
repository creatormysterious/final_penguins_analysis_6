#function to create exploratory violin plot 


# Define the function to create a violin plot with default arguments
create_exploratory_violin_plot <- function(data, 
                                           x_var = "species", 
                                           y_var = "culmen_depth_mm", 
                                           title = "Variation in Culmen Depth by Species", 
                                           x_label = "Species", 
                                           y_label = "Culmen Depth (mm)", 
                                           colors = c("Adelie" = "darkorange", 
                                                      "Chinstrap" = "purple", 
                                                      "Gentoo" = "cyan4"), 
                                           seed = 0) {
  
  # Set the random seed for reproducibility
  set.seed(seed)
  
  # Create the violin plot
  ggplot(data = data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_violin(aes(fill = species), color = NA, alpha = 0.7) +  # No outline, transparent fill
    geom_jitter(width = 0.3, size = 1.5, color = "gray26", seed = seed) +  # Solid data points
    theme_minimal() +
    labs(
      title = title,
      x = x_label,  # Adds x-axis label
      y = y_label   # Adds y-axis label
    ) +
    scale_fill_manual(values = colors) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), # Title is centered
      axis.title = element_text(size = 11),
      axis.text.x = element_blank(),  # Removes x-axis tick labels
      axis.text.y = element_text(size = 11),
      axis.ticks.x = element_blank(),  # Removes x-axis ticks
      axis.ticks.y = element_line(color = "black"),  # Ensures y-axis ticks are visible
      axis.line = element_line(color = "black"),  # Ensures axis lines are visible
      panel.grid = element_blank(),  # Removes background grid
      plot.margin = margin(10, 10, 10, 10)  # Adds space below the plot (top, right, bottom, left)
    )
}
