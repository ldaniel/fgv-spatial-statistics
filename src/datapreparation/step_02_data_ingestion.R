# performing data loading -----------------------------------------------------
dataDirectory <- "./data/raw/"
shapefile_to_read <- paste(dataDirectory, "crime_mg.shp", sep = "")

target <- shapefile(shapefile_to_read)