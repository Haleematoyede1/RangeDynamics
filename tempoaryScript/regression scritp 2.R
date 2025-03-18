#precipitation vs occupancy
library(ggplot2)

# Create a new data frame for prediction
newdata <- data.frame(meanPrecp = seq(min(reportRate$meanPrecp), 
                                      max(reportRate$meanPrecp), length.out = 100),
                      meanMaxTemp = mean(reportRate$meanMaxTemp),
                      urban_prop = mean(reportRate$urban_prop),
                      meanNDVI = mean(reportRate$meanNDVI))

# Get fitted values
newdata$predicted <- predict(model11, newdata, type = "response")


plot_predictor <- function(model, data, predictor, ylim = c(0, 1), main = NULL) {
  pred_seq <- seq(min(data[[predictor]]), max(data[[predictor]]), length.out = 100)
  
  # Create a new dataset for prediction with mean values for other predictors
  new_data <- data.frame(setNames(list(pred_seq), predictor))
  new_data[setdiff(names(data), predictor)] <- lapply(data[setdiff(names(data), predictor)], mean)
  
  # Predict values
  predictions <- predict(model, newdata = new_data, type = "response")
  
  # Plot
  plot(pred_seq, predictions, type = 'l', axes = FALSE, 
       xlab = predictor, ylab = 'Predicted Occupancy', 
       ylim = ylim, lwd = 2, cex.lab = 1.5)
  
  # Customize axis labels
  x_ticks <- quantile(pred_seq, c(0, 0.5, 1))
  axis(1, at = x_ticks, labels = round(x_ticks, 1), lwd = 2, cex.axis = 1.5)
  
  y_ticks <- seq(0, 1, 0.2)
  axis(2, at = y_ticks, labels = round(y_ticks, 2), las = 1, lwd = 2, cex.axis = 1.5)
  
  if (is.null(main)) main <- paste("Fitted Regression Line for", predictor)
  title(main = main, col.main = "black", font.main = 2)
}


# Create PDF to save the plots
pdf("Fitted_Regression_Lines_Hadeda.pdf", width = 10, height = 8)

# Set up multiple plots per page
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(1, 5, 1, 0))

# Plot for main predictors
plot_predictor(model11, reportRate, "meanPrecp")
plot_predictor(model11, reportRate, "meanMaxTemp")
plot_predictor(model11, reportRate, "meanNDVI")
plot_predictor(model11, reportRate, "urban_prop")

# Title for all plots
mtext("Fitted Regression Lines for Hadeda Occupancy", 
      outer = TRUE, side = 1, line = 0, cex = 1.2, font = 2)

# Reset plotting parameters
par(op)
dev.off()

cat("Regression plots have been saved to 'Fitted_Regression_Lines_Hadeda.pdf'\n")
