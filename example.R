# Surface geometry and biodiversity
source("R/functions.R")

dataset_list <- list(
  #"CoralReef1_100",
  "CoralReef3_150",
  "CoralReef3_200",
  "CoralReef3_250",
  "CoralReef3_300",
  "CoralReef3_350",
  "CoralReef3_400",
  "CoralReef3_450",
  "CoralReef3_500"
)

for (dataset_name in dataset_list) {
  cat("processing dataset: ", dataset_name, "\n")
  dataset = dataset_name
  
  base_folder <- "SynetheticCoralReefs/SynetheticCoralReef3_medium"
  data_file <- file.path(base_folder, dataset, paste0(dataset, ".tif"))
  print("read csv")
  coordinate_file <- file.path(base_folder, dataset, "coordinate.csv")
  print("finish csv")
  coordinate_list <- read.csv(coordinate_file, header = TRUE)
  
  print(dataset)
  
  x_coords <- coordinate_list$x
  y_coords <- coordinate_list$y
  
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
  
  cat("x0", x0, "\n")
  cat("y0", y0, "\n")
  
  # Calculate the size L as the distance between point 1 and point 2
  L <- sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
  cat("L", L, "\n")
  
  scl <- L / c(1, 2, 4, 8, 16, 32, 64, 128) # Scales, aim for 2 orders of magnitude
  L0 <- min(scl) # Grain, resolution of processing ~ 6 cm
  
  # Example surface (an 8x8m section of Horseshoe from Lizard Island)
  output <- "example" # For housekeeping
  
  # Load example geotif
  data <- raster(data_file)
  plot(data)
  
  rep <- 1
  # Choose patch in which to calculate RDH (rugosity, fractal D and height range).
  
  
  rect(x0, y0, x0+L, y0+L, border="white", lty=2)
  
  # Calulate height variation at different scales (scl) within patch, and save output (because a time-consuming step)
  print("Calculating height variation")
  output_folder <- paste0(base_folder, "_output/")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    cat("Folder created at: ", output_folder, "\n")
  } 
  
  output_path <- paste0(output_folder, names(data), ".csv")
  if (file.exists(output_path)) {
    # Do nothing
  } else {
    example <- height_variation(output_path, write=TRUE, return=TRUE)
  }
  
  # Load the file if starting here:
  example <- read.csv(output_path, as.is=TRUE)
  
  # Calculate rugosit, fractal dimension and height range (rdh function)
  print("calculating rugosity")
  result = rdh(example)
  print(result)
  
  # Convert the result (list) into a data frame
  result_df <- as.data.frame(result)
  output_path <- file.path(base_folder, dataset, "result.csv")
  
  # Write the result to a CSV file
  write.csv(result_df, file = output_path, row.names = FALSE)
  
  cat("Result saved to:", output_path, "\n")
}