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


plot_predictor <- function(model, data, predictor, means, sds, is_log_transformed = FALSE, ylim = c(0, 1), main = NULL) {
  # Step 1: Create a sequence for the predictor on the scaled scale
  pred_seq <- seq(min(data[[predictor]]), max(data[[predictor]]), length.out = 100)
  
  # Step 2: Create a new dataset for prediction
  new_data <- data.frame(setNames(list(pred_seq), predictor))
  new_data[setdiff(names(data), predictor)] <- lapply(data[setdiff(names(data), predictor)], mean)
  
  # Step 3: Predict the fitted values
  predictions <- predict(model, newdata = new_data, type = "response")
  
  # Step 4: Back-transform the predictor
  pred_seq_backscaled <- (pred_seq * sds[[predictor]]) + means[[predictor]]
  
  # Step 5: If the predictor was log-transformed before scaling, reverse the log transformation
  if (is_log_transformed) {
    pred_seq_backscaled <- exp(pred_seq_backscaled)
  }
  
  # Step 6: Plot the fitted regression line
  plot(pred_seq_backscaled, predictions, type = 'l', axes = FALSE, 
       xlab = predictor, ylab = 'Predicted Occupancy', 
       ylim = ylim, lwd = 2, cex.lab = 1.5)
  
  # Step 7: Customize the x and y axis ticks
  x_ticks <- quantile(pred_seq_backscaled, c(0, 0.5, 1))
  axis(1, at = x_ticks, labels = round(x_ticks, 1), lwd = 2, cex.axis = 1.5)
  
  y_ticks <- seq(0, 1, 0.2)
  axis(2, at = y_ticks, labels = round(y_ticks, 2), las = 1, lwd = 2, cex.axis = 1.5)
  
  # Step 8: Add plot title
  if (is.null(main)) main <- paste("Fitted Regression Line for", predictor)
  title(main = main, col.main = "black", font.main = 2)
}

# Save the plot to a PDF
pdf("Fitted_Regression_Lines_Hadeda_Backtransformed.pdf", width = 10, height = 8)

# Set up plot layout
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(1, 5, 1, 0))

# Plot main effects and interactions (log-transformed predictors marked)
plot_predictor(model11, reportRate, "meanPrecp", means, sds, is_log_transformed = TRUE)  # Precipitation was log-transformed
plot_predictor(model11, reportRate, "meanMaxTemp", means, sds)
plot_predictor(model11, reportRate, "meanNDVI", means, sds)
plot_predictor(model11, reportRate, "urban_prop", means, sds)

# Title for all plots
mtext("Fitted Regression Lines for Hadeda Occupancy (Back-transformed)", 
      outer = TRUE, side = 1, line = 0, cex = 1.2, font = 2)

# Reset plotting parameters
par(op)
dev.off()

cat("Back-transformed regression plots saved to 'Fitted_Regression_Lines_Hadeda_Backtransformed.pdf'\n")
