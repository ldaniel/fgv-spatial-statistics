# loading required libraries --------------------------------------------------
# FGVR library for data science power-ups
# library(fgvr)

# libraries for data prep
library(dplyr)
# library(readr)
# library(magrittr)
# library(forcats)
# library(lubridate)
# library(stringr)
# library(feather)
# library(fastDummies)
# library(reshape2)
# library(knitr)


# libraries for spatial data manipulation
library(rgdal)
library(raster)
library(spdep)
library(bamlss)
library(shapefiles)

# libraries for plots
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
names(target)
# AREA
# INDICE94
# INDICE95
# GINI_91
# POP_94
# POP_RUR
# POP_URB
# POP_FEM
# POP_MAS
# POP_TOT
# URBLEVEL
# PIB_PC
# X_COORD
# Y_COORD 

# getting the centroids of the polygons
xy <- coordinates(target) # getting the centroids of the polygons

# neighborhood matrix from spatial polygons / adjacent polygons ---------------

# using spdep library
ap <- poly2nb(target, row.names = target$ID)
lw <- nb2listw(ap, style = "W", zero.policy = TRUE)

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

# Global Autocorrelation Tests: Moran's I
moran.test(target$AREA, listw = lw, zero.policy = T)
moran.test(target$INDICE94, listw = lw, zero.policy = T)
moran.test(target$INDICE95, listw = lw, zero.policy = T)
moran.test(target$GINI_91, listw = lw, zero.policy = T)
moran.test(target$POP_94, listw = lw, zero.policy = T)
moran.test(as.numeric(target$POP_RUR), listw = lw, zero.policy = T)
moran.test(as.numeric(target$POP_URB), listw = lw, zero.policy = T)
moran.test(as.numeric(target$POP_FEM), listw = lw, zero.policy = T)
moran.test(as.numeric(target$POP_MAS), listw = lw, zero.policy = T)
moran.test(target$POP_TOT, listw = lw, zero.policy = T)
moran.test(target$URBLEVEL, listw = lw, zero.policy = T)
moran.test(target$PIB_PC, listw = lw, zero.policy = T)

# Moran Scatterplot
par(mar=c(4,4,1.5,0.5))
moran.plot(target$AREA, 
           listw = lw, 
           zero.policy = T,
           xlim = c(0,100),
           ylim = c(0,100), 
           pch = 16, 
           col = "black",
           cex = .5, 
           quiet = F,
           labels = as.character(target$MUNIC),
           xlab = "Percent for Area",
           ylab = "Percent for Area (Spatial Lag)", 
           main = "Moran Scatterplot")
