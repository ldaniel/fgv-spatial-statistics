# dataset preparation ---------------------------------------------------------

dataProcessedDirectory <- "./data/processed/"
shapefile_to_write <- paste(dataProcessedDirectory, "crime_mg_processed.shp", sep = "")
writeOGR(target, 
         dsn = shapefile_to_write, 
         driver ="ESRI Shapefile", 
         layer = "cities", 
         overwrite_layer = TRUE)
