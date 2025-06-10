# Surface geometry and biodiversity
source("R/functions.R")
library(terra)  # For raster operations
library(png)  # For saving PNG files

dataset_list <- list(
  #"BOU_C4_D",
  #"BOU_C13_D"
  #"DIA_C5_A",
  #"DIA_C8_D",
  #"DIA_C12_DA",
  #"DIA_C13_A",
  #"GRO_C6_A",
  #"GRO_C12_D",
  #"PIH_C7_D",
  #"PIH_C18_A",
  #"TIA_C12_DA",
  #"TIA_C24_1"
  #"UNU_C16_A",
  #"UNU_C17_DA",
  "SyntheticReef_500"
)

for (dataset_name in dataset_list) {
  cat("processing dataset: ", dataset_name, "\n")
  dataset = dataset_name
  
  base_folder <- "higres_data"
  data_file <- file.path(base_folder, dataset, paste0(dataset, ".tif"))
  coordinate_file <- file.path(base_folder, dataset, "coordinate.csv")
  
  # Load coordinates
  coordinate_list <- read.csv(coordinate_file, header = TRUE)
  
  # Print dataset name
  print(dataset)
  
  # Extract coordinates
  x_coords <- coordinate_list$x
  y_coords <- coordinate_list$y
  
  # Define rectangle points
  point1 <- c(x_coords[1], y_coords[1])
  point2 <- c(x_coords[2], y_coords[2])
  point3 <- c(x_coords[3], y_coords[3])
  point4 <- c(x_coords[4], y_coords[4])
  
  cat("point1", point1, "\n")
  cat("point2", point2, "\n")
  cat("point3", point3, "\n")
  cat("point4", point4, "\n")
  
  # Read the raster file
  raster_data <- rast(data_file)
  
  # Check the raster structure
  print(raster_data)
  
  # Plot the raster
  plot(raster_data)
  
  return
  
  # Create an extent for the rectangle (xmin, xmax, ymin, ymax)
  xmin <- min(x_coords)
  xmax <- max(x_coords)
  ymin <- min(y_coords)
  ymax <- max(y_coords)
  
  rectangle_extent <- ext(xmin, xmax, ymin, ymax)
  
  # Crop the raster to the rectangle
  cropped_raster <- crop(raster_data, rectangle_extent)
  
  # Extract values within the rectangle
  rect_values <- values(cropped_raster)
  rect_values <- rect_values[!is.na(rect_values)]  # Remove NA values
  
  # Get the range of values
  value_range <- range(rect_values, na.rm = TRUE)
  
  # Split the range into 10 groups
  breaks <- seq(value_range[1], value_range[2], length.out = 11)  # 10 groups => 11 breakpoints
  
  # Categorize values into groups
  value_groups <- cut(rect_values, breaks = breaks, include.lowest = TRUE)
  
  # Calculate the percentage of values in each group
  group_distribution <- prop.table(table(value_groups)) * 100
  
  # Print the distribution
  cat("Depth Distribution (in percentage) within the rectangle:\n")
  print(group_distribution)
  
  # Optionally, display the breaks for reference
  cat("\nBreaks used for grouping:\n")
  print(breaks)
  
  # Optional: visualize the cropped raster for verification
  plot(cropped_raster, main = "Cropped Raster Within Rectangle")
  
  output_file_non_normalized <- file.path(base_folder, dataset, "depth_map.tif")
  writeRaster(cropped_raster, output_file_non_normalized, overwrite = TRUE)
  
  
  # Normalize the raster values to a range of 0 to 1
  min_value <- min(values(cropped_raster), na.rm = TRUE)
  max_value <- max(values(cropped_raster), na.rm = TRUE)
  
  normalized_raster <- (cropped_raster - min_value) / (max_value - min_value)
  
  # Specify the output file path
  output_file <- file.path(base_folder, dataset, "normalized_depth_map.tif")
  
  # Save the normalized raster to a file
  writeRaster(normalized_raster, output_file, overwrite = TRUE)
}