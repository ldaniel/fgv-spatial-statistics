# loading required libraries ----------------------------------------------------------
# FGVR library for data science power-ups
library(fgvr)

# libraries for data prep
library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(lubridate)
library(stringr)
library(feather)
library(fastDummies)
library(reshape2)
library(knitr)

#libraries for plots
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(ggpubr)
library(plotly)

# libraries for data clean
library(VIM)
library(rms)
library(mctest)

# libraries for modeling
library(caret)
library(gmodels)
library(MASS)
library(rpart)
library(rpart.plot)
library(adabag)
library(randomForest)

# libraries for measures
library(hmeasure)
library(pROC)

# loading other scripts do be used here ----------------------------------------------
source("./src/datapreparation/step_00_config_environment.R")
source("./src/datapreparation/step_01_create_functions.R")
source("./src/datapreparation/step_02_data_ingestion.R")
source("./src/datapreparation/step_03_data_cleaning.R")
source("./src/datapreparation/step_04_label_translation.R")
source("./src/datapreparation/step_05_data_enhancement.R")
#source("./src/datapreparation/step_06_dataset_preparation.R")

# well, here is your playground. 
# feel free to do whatever you want to find out 
# the best approach for you data science project. =)
