
# create Queens contiguity matrix
spatmatrix <- poly2nb(target, queen = T)

# create a neighbours list with spatial weights
listw <- nb2listw(spatmatrix, style = "W", zero.policy = TRUE)

# calculate the local moran of the distribution of white population
lmoran <- localmoran(target$AREA, listw)
summary(lmoran)

# padronize the variable and save it to a new column
target$s_AREA <- scale(target$AREA)  %>% as.vector()

# create a spatially lagged variable and save it to a new column
target$lag_s_area <- lag.listw(listw, target$s_AREA)

# summary of variables, to inform the analysis
summary(target$s_AREA)
summary(target$lag_s_area)

# moran scatterplot, in basic graphics (with identification of influential observations)
x <- target$s_AREA
y <- target$lag_s_area %>% as.vector()
xx <- data.frame(x, y)

moran.plot(x, listw)

# moran sccaterplot, in ggplot 
# (without identification of influential observations - which is possible but requires more effort)
ggplot(xx, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = T) + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = 0, linetype = 'dashed') 

# create a new variable identifying the moran plot quadrant for each observation, dismissing the non-significant ones
target$quad_sig <- NA

# high-high quadrant
target[(target$s_AREA >= 0 & 
          target$lag_s_area >= 0) & 
         (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-high"
# low-low quadrant
target[(target$s_AREA <= 0 & 
          target$lag_s_area <= 0) & 
         (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-low"
# high-low quadrant
target[(target$s_AREA >= 0 & 
          target$lag_s_area <= 0) & 
         (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-low"
# low-high quadrant
target@data[(target$s_AREA <= 0 & 
               target$lag_s_area >= 0) & 
              (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-high"
# non-significant observations
target@data[(lmoran[, 5] > 0.05), "quad_sig"] <- "not signif."  

target$quad_sig <- as.factor(target$quad_sig)
target@data$id <- rownames(target@data)

# plotting the map
df <- fortify(target, region = "ID")
df <- left_join(df, target@data)
df %>% 
  ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
  geom_polygon(color = "black", size = .05)  + coord_equal() + 
  theme_void() + scale_fill_brewer(palette = "Set1")