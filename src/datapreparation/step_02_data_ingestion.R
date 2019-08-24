# performing data loading -----------------------------------------------------
dataRawDirectory <- "./data/raw/"
shapefile_to_read <- paste(dataRawDirectory, "crime_mg.shp", sep = "")

target <- shapefile(shapefile_to_read)