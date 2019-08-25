# performing data loading -----------------------------------------------------
dataRawDirectory <- "./data/raw/"
shapefile_to_read <- paste(dataRawDirectory, "crime_mg.shp", sep = "")

target <- readOGR(shapefile_to_read, encoding="UTF-8")
