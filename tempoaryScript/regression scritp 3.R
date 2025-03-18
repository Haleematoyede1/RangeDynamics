plot_predictor <- function(model, data, predictor, means, sds, 
                           is_log_transformed = FALSE, 
                           x_breaks = NULL, y_breaks = seq(0, 1, 0.2), 
                           ylim = c(0, 1), main = NULL) {
  
  # Step 1: Create a sequence for the predictor
  pred_seq <- seq(min(data[[predictor]], na.rm = TRUE), 
                  max(data[[predictor]], na.rm = TRUE), 
                  length.out = 100)
  
  # Step 2: Create a new dataset for prediction
  new_data <- data.frame(setNames(list(pred_seq), predictor))
  new_data[setdiff(names(data), predictor)] <- lapply(data[setdiff(names(data), predictor)], mean)
  
  # Step 3: Predict values
  predictions <- predict(model, newdata = new_data, type = "response")
  
  # Step 4: Back-transform if available in means and sds
  if (predictor %in% names(sds)) {
    pred_seq_backscaled <- (pred_seq * sds[[predictor]]) + means[[predictor]]
    if (is_log_transformed) {
      pred_seq_backscaled <- exp(pred_seq_backscaled)
    }
  } else {
    # If predictor is not scaled, use raw values
    pred_seq_backscaled <- pred_seq
  }
  
  # Step 5: Plot
  plot(pred_seq_backscaled, predictions, type = 'l', axes = FALSE,
       xlab = predictor, ylab = 'Predicted Occupancy',
       ylim = ylim, lwd = 2, cex.lab = 1.5, col = 'black')
  
  # Step 6: Add axis breaks
  if (is.null(x_breaks)) {
    x_breaks <- pretty(pred_seq_backscaled, n = 5)
  }
  
  axis(1, at = x_breaks, labels = round(x_breaks, 2), lwd = 2, cex.axis = 1.2)
  axis(2, at = y_breaks, labels = round(y_breaks, 2), las = 1, lwd = 2, cex.axis = 1.2)
  
  # Step 7: Add plot title
  if (is.null(main)) main <- paste("Fitted Regression Line for", predictor)
  title(main = main, col.main = "black", font.main = 2)
}



plot_predictor(model11, reportRate, "meanPrecp", means, sds, 
               is_log_transformed = TRUE, 
               x_breaks = c(2, 4, 6, 8))
plot_predictor(model11, reportRate, "meanMaxTemp", means, sds, 
               x_breaks = seq(20, 30, 2))
plot_predictor(model11, reportRate, "meanNDVI", means, sds, 
               x_breaks = seq(-0.2, 0.8, 0.2))
plot_predictor(model11, reportRate, "urban_prop", means, sds,
               x_breaks = seq(0, 1, 0.2))


###############################################################################
plot_interaction <- function(model, data, predictor1, predictor2, means, sds, 
                             is_log_transformed1 = FALSE, is_log_transformed2 = FALSE) {
  
  # Step 1: Create sequences for predictors
  pred_seq1 <- seq(min(data[[predictor1]], na.rm = TRUE), 
                   max(data[[predictor1]], na.rm = TRUE), 
                   length.out = 100)
  
  pred_seq2 <- c(mean(data[[predictor2]], na.rm = TRUE) - sd(data[[predictor2]], na.rm = TRUE),
                 mean(data[[predictor2]], na.rm = TRUE),
                 mean(data[[predictor2]], na.rm = TRUE) + sd(data[[predictor2]], na.rm = TRUE))
  
  # Step 2: Create a new dataset using expand.grid()
  new_data <- expand.grid(predictor1 = pred_seq1, predictor2 = pred_seq2)
  
  # Rename to match model terms
  colnames(new_data) <- c(predictor1, predictor2)
  
  # Step 3: Add mean values for other predictors
  new_data[setdiff(names(data), c(predictor1, predictor2))] <- lapply(
    data[setdiff(names(data), c(predictor1, predictor2))], mean, na.rm = TRUE)
  
  # Step 4: Predict using the model
  new_data$predicted <- predict(model, newdata = new_data, type = "response")
  
  # Step 5: Back-transform predictors (if needed)
  if (predictor1 %in% names(sds)) {
    pred_seq1_backscaled <- (pred_seq1 * sds[[predictor1]]) + means[[predictor1]]
    if (is_log_transformed1) pred_seq1_backscaled <- exp(pred_seq1_backscaled)
    
    # Repeat pred_seq1_backscaled to match row size of expand.grid()
    pred_seq1_backscaled <- rep(pred_seq1_backscaled, times = length(pred_seq2))
  } else {
    pred_seq1_backscaled <- rep(pred_seq1, times = length(pred_seq2))
  }
  
  if (predictor2 %in% names(sds)) {
    pred_seq2_backscaled <- (pred_seq2 * sds[[predictor2]]) + means[[predictor2]]
    if (is_log_transformed2) pred_seq2_backscaled <- exp(pred_seq2_backscaled)
    
    # Repeat pred_seq2_backscaled to match row size of expand.grid()
    pred_seq2_backscaled <- rep(pred_seq2_backscaled, each = length(pred_seq1))
  } else {
    pred_seq2_backscaled <- rep(pred_seq2, each = length(pred_seq1))
  }
  
  # Step 6: Plot
  ggplot(new_data, aes(x = pred_seq1_backscaled, y = predicted, color = factor(pred_seq2_backscaled))) +
    geom_line(size = 1) +
    scale_color_manual(values = c("red", "blue", "green"),
                       labels = c("-1 SD", "Mean", "+1 SD")) +
    labs(x = predictor1, y = "Predicted Occupancy", color = predictor2) +
    theme_minimal()
}

plot_interaction(model11, reportRate, "meanPrecp", "meanMaxTemp", means, sds,
                 is_log_transformed1 = TRUE)
plot_interaction(model11, reportRate, "meanNDVI", "urban_prop", means, sds)
