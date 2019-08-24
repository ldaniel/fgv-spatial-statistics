# loading required libraries ----------------------------------------------------------
# FGVR library for data science power-ups
# library(fgvr)

# libraries for spatial data manipulation
library(rgdal)
library(raster)
library(spdep)

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

# loading other scripts do be used here ----------------------------------------------
source("./src/datapreparation/step_00_config_environment.R")
source("./src/datapreparation/step_01_create_functions.R")
source("./src/datapreparation/step_02_data_ingestion.R")
# source("./src/datapreparation/step_03_data_cleaning.R")
# source("./src/datapreparation/step_04_label_translation.R")
# source("./src/datapreparation/step_05_data_enhancement.R")
#source("./src/datapreparation/step_06_dataset_preparation.R")


# doing some spatial exploratory analysis -------------------------------------
View(target)
plot(target)
xy <- coordinates(target) # getting the centroids of the polygons

# adjacent polygons
w <- poly2nb(target, row.names = target$ID)
class(w)
summary(w)
str(w)

plot(target, col = 'cadetblue2', border = 'deepskyblue4', lwd = 2)
plot(w, xy, col = 'red', lwd = 2, add = TRUE)