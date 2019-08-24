# loading required libraries --------------------------------------------------
# FGVR library for data science power-ups
# library(fgvr)

# libraries for spatial data manipulation
library(rgdal)
library(raster)
library(spdep)
library(bamlss)
library(shapefiles)

# libraries for data prep
# library(dplyr)
# library(readr)
# library(magrittr)
# library(forcats)
# library(lubridate)
# library(stringr)
# library(feather)
# library(fastDummies)
# library(reshape2)
# library(knitr)

# ibraries for plots
# library(ggplot2)
# library(ggthemes)
# library(ggcorrplot)
# library(ggpubr)
# library(plotly)

# libraries for data clean
# library(VIM)
# library(rms)
# library(mctest)

# libraries for modeling
# library(caret)
# library(gmodels)
# library(MASS)
# library(rpart)
# library(rpart.plot)
# library(adabag)
# library(randomForest)

# libraries for measures
# library(hmeasure)
# library(pROC)

# loading other scripts do be used here ---------------------------------------
source("./src/datapreparation/step_00_config_environment.R")
source("./src/datapreparation/step_01_create_functions.R")
source("./src/datapreparation/step_02_data_ingestion.R")
source("./src/datapreparation/step_03_data_cleaning.R")
# source("./src/datapreparation/step_04_label_translation.R")
# source("./src/datapreparation/step_05_data_enhancement.R")
#source("./src/datapreparation/step_06_dataset_preparation.R")


# doing some spatial exploratory analysis -------------------------------------
View(target)
plot(target)

# getting the centroids of the polygons
xy <- coordinates(target) # getting the centroids of the polygons

# neighborhood matrix from spatial polygons / adjacent polygons ---------------

# using spdep library
ap <- poly2nb(target, row.names = target$ID)
class(ap)
summary(ap)
str(ap)

plot(target, col = 'cadetblue2', border = 'deepskyblue4', lwd = 2)
plot(ap, xy, col = 'red', lwd = 2, add = TRUE)

# using bamlss library
nm <- neighbormatrix(target)
print(nm)
plotneighbors(target)
plotneighbors(target, type = "delaunay")
plotneighbors(target, type = "dist", d1 = 0, d2 = 0.15)