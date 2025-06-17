# Surface geometry and biodiversity
source("R/functions.R")
library(terra)  # For raster operations
library(png)  # For saving PNG files

dataset_list <- list(
  "SynetheticCoralReef5_medium"
)

for (dataset_name in dataset_list) {
  cat("processing dataset: ", dataset_name, "\n")
  dataset = dataset_name
  sub_folders <- list.dirs(dataset_name, recursive = TRUE, full.names = TRUE)
  filtered_folders <- sub_folders[file.info(sub_folders)$isdir]
  filtered_folders <- sub_folders[sub_folders != dataset_name]
  print(filtered_folders)
  
  for (folder in filtered_folders) {
    folder <- basename(folder)
    cat("processing folder: ", folder, "\n")
    data_file <- file.path(dataset_name, folder, paste0(folder, ".tif"))
    coordinate_file <- file.path(dataset_name, folder, "coordinate.csv")
    
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
    
    
    # Combine points into a matrix for easier processing
    points <- rbind(point1, point2, point3, point4)
    
    # Calculate the bottom-right corner (x0, y0)
    x0 <- min(points[, 1]) # Minimum x-coordinate
    y0 <- min(points[, 2]) # Minimum y-coordinate
    x1 <- max(points[, 1]) # Minimum x-coordinate
    y1 <- max(points[, 2]) # Minimum y-coordinate
    L <- sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
    cat("L", L, "\n")
    # Read the raster file
    raster_data <- raster(data_file)
    
    # Check the raster structure
    print(raster_data)
    
    # Plot the raster
    plot(raster_data)
    
    bx <- extent(x0, x1, y0, y1)
    
    cropped_raster = crop(raster_data, bx)
    
    
    # Optional: visualize the cropped raster for verification
    plot(cropped_raster, main = "Cropped Raster Within Rectangle")
    
    output_file_non_normalized <- file.path(dataset_name, folder, "depth_map.tif")
    writeRaster(cropped_raster, output_file_non_normalized, overwrite = TRUE)
    
    
    # Normalize the raster values to a range of 0 to 1
    min_value <- min(values(cropped_raster), na.rm = TRUE)
    max_value <- max(values(cropped_raster), na.rm = TRUE)
    
    normalized_raster <- (cropped_raster - min_value) / (max_value - min_value)
    
    # Specify the output file path
    output_file <- file.path(dataset_name, folder, "normalized_depth_map.tif")
    
    # Save the normalized raster to a file
    writeRaster(normalized_raster, output_file, overwrite = TRUE) 
  }
}