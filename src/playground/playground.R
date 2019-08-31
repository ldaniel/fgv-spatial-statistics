# loading required libraries --------------------------------------------------
# FGVR library for data science power-ups
# library(fgvr)

# libraries for data prep
library(dplyr)
# library(readr)
# library(magrittr)
# library(forcats)
# library(lubridate)
library(stringr)
# library(feather)
# library(fastDummies)
# library(reshape2)
# library(knitr)


# libraries for spatial data manipulation
library(spatialreg)
library(maps)
library(maptools)    
library(rgdal)     
library(sp)  
library(spdep)
library(bamlss)
library(gstat)
library(splancs)
library(spatstat)
library(pgirmess)
library(classInt)
library(spgwr)

# libraries for plots and visualization
library(RColorBrewer)
library(tmap)
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
source("./src/datapreparation/step_04_label_translation.R")
source("./src/datapreparation/step_05_data_enhancement.R")
source("./src/datapreparation/step_06_dataset_preparation.R")

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

#TO-DO: Checar porque está faltando os polígonos de Juramento e Jesuânia

# Pergunta 1 ------------------------------------------------------------------
# Qual das variáveis quantitativas apresentadas no shapefile crime_mg apresenta 
# maior auto-correlação espacial? Descreva como implementou a matriz de 
# vizinhança. Apresente o I de Moran e o mapa de auto-correlação espacial local 
# (LISA map) da variável escolhida e também de pelo menos outras 3 variáveis.
# Obs: desconsidere as variáveis Codmuni, ID, X_coord e Y_coord nessa análise.

# getting the centroids of the polygons
xy <- coordinates(target) 

# neighborhood matrix from spatial polygons / adjacent polygons

# using the spdep library
ap <- poly2nb(target, queen = T)
lw <- nb2listw(ap, style = "W", zero.policy = TRUE)

class(ap)
summary(ap)
str(ap)

plot(target, col = 'cadetblue2', border = 'deepskyblue4', lwd = 1)
plot(ap, xy, col = 'red', lwd = 2, add = TRUE)

# using the bamlss library
nm <- neighbormatrix(target)
print(nm)
plotneighbors(target)
plotneighbors(target, type = "delaunay")
plotneighbors(target, type = "dist", d1 = 0, d2 = 0.15)

# global autocorrelation tests: Moran's I
moran.test.AREA     <- moran.test(target$AREA, listw = lw, zero.policy = T) 
moran.test.INDICE94 <- moran.test(target$INDICE94, listw = lw, zero.policy = T)
moran.test.INDICE95 <- moran.test(target$INDICE95, listw = lw, zero.policy = T)
moran.test.GINI_91  <- moran.test(target$GINI_91, listw = lw, zero.policy = T)
moran.test.POP_94   <- moran.test(target$POP_94, listw = lw, zero.policy = T)
moran.test.POP_RUR  <- moran.test(target$POP_RUR, listw = lw, zero.policy = T)
moran.test.POP_URB  <- moran.test(target$POP_URB, listw = lw, zero.policy = T)
moran.test.POP_FEM  <- moran.test(target$POP_FEM, listw = lw, zero.policy = T)
moran.test.POP_MAS  <- moran.test(target$POP_MAS, listw = lw, zero.policy = T)
moran.test.POP_TOT  <- moran.test(target$POP_TOT, listw = lw, zero.policy = T)
moran.test.URBLEVEL <- moran.test(target$URBLEVEL, listw = lw, zero.policy = T)
moran.test.PIB_PC   <- moran.test(target$PIB_PC, listw = lw, zero.policy = T)

moran.test.all <- rbind(t(data.frame("AREA" = moran.test.AREA$estimate)),
                        t(data.frame("INDICE94" = moran.test.INDICE94$estimate)),
                        t(data.frame("INDICE95" = moran.test.INDICE95$estimate)),
                        t(data.frame("GINI_91" = moran.test.GINI_91$estimate)),
                        t(data.frame("POP_94" = moran.test.POP_94$estimate)),
                        t(data.frame("POP_RUR" = moran.test.POP_RUR$estimate)),
                        t(data.frame("POP_URB" = moran.test.POP_URB$estimate)),
                        t(data.frame("POP_FEM" = moran.test.POP_FEM$estimate)),
                        t(data.frame("POP_MAS" = moran.test.POP_MAS$estimate)),
                        t(data.frame("POP_TOT" = moran.test.POP_TOT$estimate)),
                        t(data.frame("URBLEVEL" = moran.test.URBLEVEL$estimate)),
                        t(data.frame("PIB_PC" = moran.test.PIB_PC$estimate)))

print(moran.test.all)

# Moran scatterplot for AREA
par(mar = c(4,4,1.5,0.5))
moran.plot(target$AREA, 
           listw = lw, 
           zero.policy = T,
           pch = 16, 
           col = "black",
           cex = .5, 
           quiet = F,
           labels = as.character(target$MUNIC),
           xlab = "Percent for Area",
           ylab = "Percent for Area (Spatial Lag)", 
           main = "Moran Scatterplot")

# LISA map for AREA
locm <- localmoran(target$AREA, lw)

target$sPPOV <- scale(target$AREA)
target$lag_sPPOV <- lag.listw(lw, target$sPPOV)

plot(x = target$sPPOV, y = target$lag_sPPOV, main = " Moran Scatterplot PPOV")
abline(h = 0, v = 0)
abline(lm(target$lag_sPPOV ~ target$sPPOV), lty = 3, lwd = 4, col = "red")

# check out the outliers click on one or two and then hit escape or click finish
identify(target$sPPOV, target$lag_sPPOV, target$AREA, cex = 0.8)

target$quad_sig <- NA
target@data[(target$sPPOV >= 0 & target$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
target@data[(target$sPPOV <= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
target@data[(target$sPPOV <= 0 & target$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 5 

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(target$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
par(mar = c(4,0,4,1))
plot(target, col = colors[np])
mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")

# Pergunta 2 ------------------------------------------------------------------
# Implemente o modelo espacial auto-regressivo (SAR) da variável Indice95 
# (índice de criminalidade em 1995 de Minas Gerais) a partir de apenas uma 
# variável independente (não pode ser Indice94, Codmuni, ID, X_coord nem 
# Y_coord). Apresente o resultado da regressão linear simples e da regressão 
# linear espacial. Apresente as equações e interprete seus coeficientes.

# initial setup
res.palette <- colorRampPalette(c("red","orange","white","lightgreen","green"), 
                                space = "rgb")
pal <- res.palette(5)
par(mar = c(2, 0, 4, 0))

# linear regresion model
target.lm.model <- lm(INDICE95 ~ AREA, data = target)
summary(target.lm.model)

target.lm.model.residuals <- target.lm.model$residuals

target.lm.model.class_fx <- classIntervals(target.lm.model.residuals, 
                                           n = 5,
                                           style = "fixed",
                                           fixedBreaks = c(-50,-25,-5,5,25,50),
                                           rtimes = 1)

cols.lm <- findColours(target.lm.model.class_fx, pal)

plot(target, col = cols.lm, main = "OLS Model", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols, "palette"), bty = "n",
       legend = names(attr(cols, "table")), title = "Residuals from OLS Model",
       ncol = 5)

moran.test(target.lm.model.residuals, listw = lw, zero.policy = T)

# SAR model (Spatial Auto-Regressive model)
target.sar.model <- lagsarlm(INDICE95 ~ AREA, 
                             data = target, 
                             listw = lw,
                             zero.policy = T, 
                             tol.solve = 1e-12)
summary(target.sar.model)

target.sar.model.residuals <- target.sar.model$residuals

target.sar.model.class_fx <- classIntervals(target.sar.model.residuals, 
                                            n = 5, 
                                            style = "fixed",
                                            fixedBreaks = c(-50,-25,-5,5,25,50),
                                            rtimes = 1)

cols.sar <- findColours(target.sar.model.class_fx, pal)

plot(target, col = cols.sar, main = "SAR Model", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols, "palette"), bty = "n",
       legend = names(attr(cols, "table")), title = "Residuals from SAR Model",
       ncol = 5)

moran.test(target.sar.model.residuals, listw = lw, zero.policy = T)

# Pergunta 3 ------------------------------------------------------------------
# Para essa variável que você escolheu, o modelo espacial SAR apresentou ganhos 
# significantes com relação ao modelo linear simples? Justifique sua resposta.
# Obs: Sugere-se fazer essa atividade no GeoDA ou no R.

# Pergunta 4 ------------------------------------------------------------------
# Implemente a regressão espacial GWR da variável Indice95 (índice de 
# criminalidade em 1995 de Minas Gerais) a partir de apenas uma variável 
# independente (não pode ser Indice94, Codmuni, ID, X_coord nem Y_coord). 
# Apresente o resultado da regressão linear simples e da regressão linear 
# espacial por GWR. Apresente medidas da distribuição dos coeficientes 
# (min, Q1, Q2, Q3, máx), e da distribuição do R2 (min, Q1, Q2, Q3, máx) e 
# apresente os resultados globais da regressão (R2 global, basicamente).
# Obs: Sugere-se fazer essa atividade no ArcGIS ou no R.

# Pergunta 5 ------------------------------------------------------------------
# Para essa variável que você escolheu, o modelo espacial GWR apresentou ganhos 
# significantes com relação ao modelo linear simples? Justifique sua resposta.

# Pergunta 6 ------------------------------------------------------------------
# Implemente um modelo de regressão linear multivariado stepwise da variável 
# Indice95 (significante a 5% ou 10%, utilize o que achar melhor). Depois, 
# “promova-o” a um modelo SAR. Apresente os resultados comparados (equação, 
# R2). Qual modelo você escolheria como final? Se desejar, apresente mapas 
# que sustente sua justificativa.

# Pergunta 7 (bônus) ----------------------------------------------------------
# Promova o modelo final linear da Pergunta 6 a um modelo GWR. Apresente os 
# resultados comparados (equação, R2). Qual modelo você escolheria como final? 
# Se desejar, apresente mapas que sustente sua justificativa.

# Pergunta 8 (bônus 2) --------------------------------------------------------
# Produza um mapa de alta qualidade do shapefile crime_mg utilizando a extensão 
# tmap. Apresente o código completo e o mapa produzido em sua resposta.

# interactive map
tmap_mode("view")
tm_shape(target) +
  tm_polygons("INDICE95", 
              id = "MUNIC", 
              title = "indice 1995", 
              contrast = 0.7,
              palette = "-Blues",
              border.col = "gray30") +
  tm_format("World") 

# classic map
tmap_mode("plot")
tm_shape(target) +
  tm_polygons("INDICE95", id = "MUNIC") +
  tm_compass(position = c(.1, .1), color.light = "grey90") +
  tm_credits("Eckert IV projection", position = c("RIGHT", "BOTTOM")) +
  tm_style("classic")

