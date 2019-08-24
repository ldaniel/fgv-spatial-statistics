# metrics functions -----------------------------------------------------------

# The objective of this function is to calculate main metrics of model performance according to a cutoff value.
calculateModelMetrics <- function(cutData, realData, predData){
  cuttedData <- as.factor(ifelse(predData>=cutData, 1, 0))
  
  invisible(capture.output(out <- CrossTable(realData, cuttedData, 
                                             prop.c = F, prop.t = F, prop.r = T, prop.chisq = F)))
  
  out <- as.data.frame(out) %>% 
    mutate(merged=paste0(t.x, t.y)) %>% 
    dplyr::select(merged, val=t.Freq)
  
  TN <- filter(out, merged == "00")$val[1]
  FP <- filter(out, merged == "01")$val[1]
  FN <- filter(out, merged == "10")$val[1]
  TP <- filter(out, merged == "11")$val[1]
  
  return(data.frame(Cut = cutData,
                    TN = TN, 
                    FP = FP,
                    FN = FN, 
                    TP = TP,
                    TPR = TP/(TP+FN), TNR=TN/(TN+FP),
                    Error = (FP+FN)/(TP+TN+FP+FN),
                    Precision = TP/(TP+FP),
                    F1 = 2*(TP/(TP+FN))*(TP/(TP+FP))/((TP/(TP+FP)) + (TP/(TP+FN)))))
}

# The objective of this function is to calculate main metrics of model performance 
# for cutoffs from 0-1 based on given step.
modelMetrics <- function(realData, predData, stepping = 0.01, 
                         plot_title = "TPR/TNR by cutoff over full dataset"){
  probCuts <- seq(from = 0, to = 1, by = stepping)
  out <- bind_rows(lapply(probCuts, calculateModelMetrics, realData = realData, predData = predData))
  out <- out[complete.cases(out),] %>% mutate(Difference = abs(TPR-TNR))
  
  best <- out %>% arrange(Difference) %>% head(1) %>% dplyr::select(-Difference)
  
  p <- plot_ly(x = ~out$Cut, y = ~out$Difference, name = 'Abs. Diff.', type = 'bar', opacity = 0.3) %>% 
    add_trace(x = ~out$Cut, y = ~out$TPR, name = 'TPR', type = 'scatter', mode = 'lines', opacity = 1) %>% 
    add_trace(x = ~out$Cut, y = ~out$TNR, name = 'TNR', type = 'scatter', mode = 'lines', opacity = 1) %>% 
    layout(xaxis = list(title = "Cutoff Value"),
           yaxis = list(title = "True Ratio (%)")) %>%
    add_annotations(
      text = sprintf("<b>%s</b>", plot_title),
      x = 0,
      y = 1.04,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 15)
    ) %>%
    add_annotations(
      text = sprintf("<b>%s</b>", best$Cut),
      x = best$Cut,
      y = best$TPR,
      showarrow = FALSE,
      bgcolor = "white",
      opacity = 0.8
    )
  
  return(list(TableResults = out,
              BestCut = best,
              Plot = p))
}

# accuracy ----------------------------------------------------------------
accuracy <- function(score, actual, threshold = 0.5) {
  
  fitted.results <- ifelse(score > threshold ,1 ,0)
  
  misClasificError <- mean(fitted.results != actual)
  
  misClassCount <- misclassCounts(fitted.results, actual)
  
  print(kable(misClassCount$conf.matrix))
  
  print('--------------------------------------------------------------')
  print(paste('Model General Accuracy of: ', 
              round((1 - misClassCount$metrics['ER']) * 100, 2), '%', 
              sep = ''))
  print(paste('True Positive Rate of    : ', 
              round(misClassCount$metrics['TPR'] * 100, 2), '%',
              sep = ''))
}

# plot functions --------------------------------------------------------------
# functions used in the evaluation step to compare the models.

Score_Histograms <- function(dataset, predicted, actual, title) {
  ggplot(data = dataset) +
    geom_density(aes(x = predicted, fill = as.factor(actual)),
                 alpha = 0.5) +
    scale_fill_manual(values = c("0" = "#16a085", "1" = "#e74c3c")) +
    scale_x_continuous(limits = c(0, 1)) +
    theme_economist() +
    labs(title = title,
         y = 'Score',
         fill = 'Defaulter |1 = True|') +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}

Score_Boxplot <- function(dataset, predicted, actual, title) {
  ggplot(data = dataset) +
    geom_boxplot(aes(y = predicted,
                     fill = as.factor(actual))) +
    coord_flip() +
    scale_fill_manual(values = c("0" = "#16a085", "1" = "#e74c3c")) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_economist() +
    labs(title = title,
         y = 'Score',
         fill = 'Defaulter |1 = True|') +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}

KS_Plot <- function(zeros, ones, title) {
  group <- c(rep("Non Defaulters", length(zeros)), rep("Defauters", length(ones)))
  dat <- data.frame(KSD = c(zeros, ones), group = group)
  cdf1 <- ecdf(zeros) 
  cdf2 <- ecdf(ones) 
  minMax <- seq(min(zeros, ones), max(zeros, ones), length.out=length(zeros)) 
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )][1] 
  y0 <- cdf1(x0)[1]
  y1 <- cdf2(x0)[1]
  ks <- round(y0 - y1, 2)
  
  ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "blue") +
    geom_point(aes(x = x0[1] , y = y0[1]), color="blue", size=4) +
    geom_point(aes(x = x0[1] , y = y1[1]), color="blue", size=4) +
    geom_label(aes(x = x0[1], y = y1[1] + (y0[1] - y1[1]) / 2, label = ks),
               color = 'black') +
    scale_x_continuous(limits = c(0, 1)) +
    labs(title = title,
         y = 'Cumulative Probability Distribution',
         x = 'Score') +
    theme_economist() +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = 0,
          plot.title = element_text(hjust = 0.5))
}

Plot_ROC <- function(dataset, smooth_opt = FALSE) {
  roc_logistic      <- roc(logistic.actual ~ logistic.predicted,
                           dataset,
                           smooth = smooth_opt,
                           quiet = TRUE)
  
  roc_decision.tree <- roc(decision.tree.actual ~ decision.tree.predicted,
                           dataset,
                           smooth = smooth_opt,
                           quiet = TRUE)
  
  roc_boosting      <- roc(boosting.actual ~ boosting.predicted,
                           dataset,
                           smooth = smooth_opt,
                           quiet = TRUE)
  
  roc_random.forest <- roc(random.forest.actual ~ random.forest.predicted,
                           dataset,
                           smooth = smooth_opt,
                           quiet = TRUE)
  
  p <- ggplot() +
    geom_line(aes(x = 1 - roc_logistic$specificities, 
                  y = roc_logistic$sensitivities, 
                  colour = 'Logistic Regression'), # red
              size = 1,
              linetype = 1,
              alpha = 0.7) +
    geom_line(aes(x = 1 - roc_decision.tree$specificities, 
                  y = roc_decision.tree$sensitivities,
                  colour = 'Decision Tree'), # blue
              size = 1,
              linetype = 1,
              alpha = 0.7) +
    geom_line(aes(x = 1 - roc_boosting$specificities, 
                  y = roc_boosting$sensitivities,
                  colour = 'Boosting'), # green
              size = 1,
              linetype = 1,
              alpha = 0.7) +
    geom_line(aes(x = 1 - roc_random.forest$specificities, 
                  y = roc_random.forest$sensitivities,
                  colour = 'Random Forest'), # purple
              size = 2,
              linetype = 1,
              alpha = 1) +
    geom_abline(aes(intercept = 0, slope = 1),
                linetype = 2,
                size = 1) +
    scale_colour_manual(name = NULL,
                        breaks = c('Logistic Regression', 
                                   'Decision Tree',
                                   'Boosting', 
                                   'Random Forest'),
                        labels = c('Logistic Regression', 
                                   'Decision Tree',
                                   'Boosting', 
                                   'Random Forest'),
                        values = c('#C0392B', 
                                   '#3498DB', 
                                   '#28B463', 
                                   '#9B59B6')) +
    labs(y = 'True Positive Rate',
         x = 'False Positive Rate',
         title = 'Receiver Oerating Characteristic Curve - ROC',
         subtitle = 'Random Forest and Boosting are the models that best discriminate Defaulters and Non-Defaulters') +
    theme_economist() +
    theme(panel.grid = element_blank())
  
  return (p)
}
