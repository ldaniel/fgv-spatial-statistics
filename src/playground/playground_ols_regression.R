library(ggExtra)

# initial exploration in INDICE95 x AREA
indice95_by_urblevel_plot <- ggplot(data = target@data, 
                                    aes(x = target$INDICE95, 
                                        y = target$AREA, 
                                        color = target$URBLEVEL)) +
  geom_point() +
  theme(legend.position = "none") +
  xlab("INDICE95") +
  ylab("AREA")

ggMarginal(indice95_by_urblevel_plot, type = "histogram")

# runing the linear model multivaluated and looking at the residuals
target.ols.model <- lm(INDICE95 ~ 
                         AREA + 
                         INDICE94 + 
                         GINI_91 +
                         POP_94 +
                         POP_RUR +
                         POP_URB +
                         POP_FEM +
                         POP_MAS +
                         POP_TOT +
                         URBLEVEL +
                         PIB_PC, 
                       data = target)

summary(target.ols.model)

target$resid <- residuals(target.ols.model)
spplot(target, "resid", main = "Residuals")

# runing the error SAR model and looking at the error terms
target.errorsar.model <- errorsarlm(formula =INDICE95 ~ 
                                      AREA + 
                                      INDICE94 + 
                                      GINI_91 +
                                      POP_94 +
                                      POP_RUR +
                                      POP_URB +
                                      POP_FEM +
                                      POP_MAS +
                                      POP_TOT +
                                      URBLEVEL +
                                      PIB_PC, 
                                    data = target,
                                    listw = lw, 
                                    quiet = T)

summary(target.errorsar.model)

target$fitted_sem <- target.errorsar.model$fitted.values
spplot(target, "fitted_sem", main = "Trend")

target$resid_sem <- target.errorsar.model$residuals
spplot(target, "resid_sem", main = "Residuals")


# -----------------------------------------------------------------------------

library(spselect)

data(y)
data(X.3D)
y.name <- "y"
ss <- c("ind", "ss1", "ss2")
mod_forward.step.ss_1 <- stepwise.ss(y, X.3D, y.name, ss, 1, verbose = T)