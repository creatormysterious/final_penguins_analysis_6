#code for 'save' function, which saves figures as svg files 

save_plot_svg <- function(plot, filename, size, scaling, folder = "figures") {
  # Convert size from cm to inches (1 inch = 2.54 cm)
  size_inches = size / 2.54
  
  # Set up SVG output
  svglite(filename, width = size_inches, 
          height = size_inches, 
          scaling = scaling)
  
  # Print the plot to the device
  print(plot)
  
  # Close the device and save the plot
  dev.off()
}


