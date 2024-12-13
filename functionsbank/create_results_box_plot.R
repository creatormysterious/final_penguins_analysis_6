#function to create results box plot

create_box_plot <- function(data, 
                            tukey_results,  # Accept Tukey results as an input parameter
                            x_var = "species", 
                            y_var = "culmen_depth_mm", 
                            title = "Comparing Average Culmen Depth Across Species", 
                            x_label = "Species", 
                            y_label = "Culmen Depth (mm)", 
                            colors = c("Adelie" = "darkorange", 
                                       "Chinstrap" = "purple", 
                                       "Gentoo" = "cyan4"), 
                            comparisons = list(c("Adelie", "Chinstrap"), 
                                               c("Adelie", "Gentoo"), 
                                               c("Chinstrap", "Gentoo")),
                            textsize = 4) {
  
  # Extract p-values for the species comparisons from Tukey results
  tukey_species_results <- tukey_results$species
  
  # If tukey_species_results is a matrix or data frame, extract p-values
  if (is.data.frame(tukey_species_results)) {
    tukey_p_values <- tukey_species_results$p.adj
  } else if (is.matrix(tukey_species_results)) {
    tukey_p_values <- tukey_species_results[, "p adj"]
  }
  
  # Create custom annotations based on Tukey p-values
  annotations <- sapply(tukey_p_values, function(p) {
    if (p < 0.001) return("***")
    else if (p < 0.01) return("**")
    else if (p < 0.05) return("*")
    else return(NA)  # If p-value is not significant, don't add stars
  })
  
  # Ensure annotations have the same length as comparisons
  if (length(annotations) != length(comparisons)) {
    stop("The number of annotations does not match the number of comparisons.")
  }
  
  # Create the box plot
  ggplot(data = data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_boxplot(aes(fill = species), width = 0.5, outlier.shape = NA, alpha = 0.7) +  # Box plot with no outliers and reduced width
    theme_minimal() +
    labs(
      title = title,
      x = x_label,  
      y = y_label   
    ) +
    scale_fill_manual(values = colors) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), 
      axis.title = element_text(size = 11),  
      axis.text.x = element_blank(),         
      axis.ticks.x = element_blank(),        
      axis.text.y = element_text(size = 11), 
      axis.ticks = element_line(color = "black"), 
      axis.line = element_line(color = "black"),
      panel.grid = element_blank()
    ) +
    geom_signif(
      comparisons = comparisons,            
      map_signif_level = FALSE,             
      step_increase = 0.1,                  
      color = "black",                       
      textsize = textsize,                   
      na.rm = TRUE,                         
      annotations = annotations              
    ) 
}




