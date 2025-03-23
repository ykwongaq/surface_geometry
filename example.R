# Surface geometry and biodiversity
source("R/functions.R")

dataset_list <- list(
  #"BOU_C4_D"
  #"BOU_C13_D"
  #"DIA_C5_A"
  #"DIA_C8_D"
  #"DIA_C12_DA"
  #"DIA_C13_A"
  #"GRO_C6_A"
  #"GRO_C12_D"
  #"PIH_C7_D"
  #"PIH_C18_A"
  #"TIA_C12_DA"
  #"TIA_C24_1"
  #"UNU_C16_A"
  "UNU_C17_DA"
)

for (dataset_name in dataset_list) {
  cat("processing dataset: ", dataset_name, "\n")
  dataset = dataset_name
  
  base_folder <- "data"
  data_file <- file.path(base_folder, dataset, paste0(dataset, ".tif"))
  coordinate_file <- file.path(base_folder, dataset, "coordinate.csv")
  
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
  x0 <- min(points[, 1]) # Maximum x-coordinate
  y0 <- min(points[, 2]) # Minimum y-coordinate
  
  # Calculate the size L as the distance between point 1 and point 2
  L <- sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
  cat("L", L)
  
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
  output_path <- paste0("output/", output, "/var_", names(data), "_0001.csv")
  if (file.exists(output_path)) {
    # Do nothing
  } else {
    example <- height_variation(write=TRUE, return=TRUE)
  }
  
  # Load the file if starting here:
  example <- read.csv(paste0("output/", output, "/var_", names(data), "_0001.csv"), as.is=TRUE)
  
  # Calculate rugosit, fractal dimension and height range (rdh function)
  print("calculating rugosity")
  result = rdh(example)
  print(result)
}